{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
import Yesod.Core
import Yesod.Form
import Text.Hamlet
import Control.Monad.IO.Class (liftIO)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import Data.Version
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HMap
import Data.Map (Map)
import Text.ParserCombinators.ReadP hiding (string)
import System.Directory
import System.Environment
import Data.Set (Set, toAscList)
import qualified Data.Set as Set
import Control.Concurrent.MVar
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Codec.Archive.Tar (Entries (..), entryPath, entryContent, EntryContent (NormalFile))
import Codec.Compression.GZip (decompress, compress)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import Data.Yaml hiding (array)
import Control.Monad (join, unless)
import System.Console.CmdArgs
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost)
import Network.HTTP.Types (status403)
import qualified Data.Text as T
import Text.Blaze.Html (toHtml)
import qualified Data.Vector as Vector
import Data.Conduit (($$))
import Data.Conduit.List (consume)

data Args = Args
    { port :: Int
    , password :: Maybe String
    , localhost :: Bool
    , rootdir :: Maybe String
    , title :: String
    } deriving (Show, Data, Typeable)

type CabalFile = FilePath
type Tarball = FilePath

data Yackage = Yackage
    { rootDir :: FilePath
    , packages :: MVar PackageDB
    , ypassword :: Maybe String
    , ytitle :: String
    }

type PackageDB = Map PackageName (Set Version)

mkYesod "Yackage" [parseRoutes|
/ RootR GET POST
/00-index.tar.gz IndexR GET
/package/#String TarballR GET
|]

tarballR :: PackageName -> Version -> Route Yackage
tarballR pn v = TarballR $ tarballName pn v

tarballName pn v = concat
    [ T.unpack $ toPathPiece pn
    , "-"
    , T.unpack $ toPathPiece v
    , ".tar.gz"
    ]

cabalName pn v = concat
    [ T.unpack $ toPathPiece pn
    , "-"
    , T.unpack $ toPathPiece v
    , ".cabal"
    ]

tarballPath pn v = do
    rd <- rootDir `fmap` getYesod
    return $ concat
        [ rd
        , '/' : T.unpack (toPathPiece pn)
        , '/' : T.unpack (toPathPiece v)
        ]

instance Yesod Yackage
instance PathPiece Version where
    fromPathPiece s =
        case filter (\(_, y) -> null y) $ readP_to_S parseVersion $ T.unpack s of
            [] -> Nothing
            (x, ""):_ -> Just x
    toPathPiece = T.pack . showVersion
instance PathPiece PackageName where
    fromPathPiece = Just . PackageName . T.unpack
    toPathPiece = T.pack . unPackageName'

getRootR :: Handler RepHtml
getRootR = do
    y <- getYesod
    ps <- getYesod >>= liftIO . readMVar . packages >>= return . Map.toList
    defaultLayout $ do
        setTitle $ toHtml $ ytitle y
        toWidget [hamlet|
<h1>#{ytitle y}
<form method="post" enctype="multipart/form-data">
    <div>
        \Upload a new file: 
        <input type="file" name="file">
    $maybe _ <- ypassword y
        <div>
            \Password: 
            <input type="password" name="password">
    <div>
        <input type="submit" value="Upload">
<dl>
    $forall p <- ps
        <dt>#{unPackageName' (fst p)}
        <dd>
            $forall v <- toAscList (snd p)
                <a href="@{tarballR (fst p) v}">#{toPathPiece v}
                \ 
|]

instance RenderMessage Yackage FormMessage where
    renderMessage _ _ = defaultFormMessage

postRootR = do
    y <- getYesod
    case ypassword y of
        Nothing -> return ()
        Just p -> do
            p' <- runInputPost $ iopt textField "password"
            unless (Just (T.pack p) == p') $ permissionDenied "Invalid password"
    (_, files) <- runRequestBody
    content <-
        case lookup "file" files of
            Nothing -> error "No file upload found"
            Just fi -> fmap L.fromChunks $ fileSource fi $$ consume
    let entries = Tar.read $ decompress content
    let cabal =
            case getCabal entries of
                NormalFile lbs _ -> lbs
                _ -> error "cabal file must be a NormalFile"
    let di =
            case parsePackageDescription $ unpack $ decodeUtf8With lenientDecode cabal of
                ParseOk _ x -> x
                y -> error $ "Invalid cabal file: " ++ show y
    let pi = package $ packageDescription di
    let name = pkgName pi
    let version = pkgVersion pi
    tp <- tarballPath name version
    rd <- fmap rootDir getYesod
    liftIO $ createDirectoryIfMissing True tp
    liftIO $ L.writeFile (rd ++ "/package/" ++ tarballName name version) content
    liftIO $ L.writeFile (tp ++ '/' : cabalName name version) cabal

    mpackages <- fmap packages getYesod
    ps <- liftIO $ takeMVar mpackages
    let oldSet = fromMaybe Set.empty $ Map.lookup name ps
    let newSet = Set.insert version oldSet
    let ps' = Map.insert name newSet ps
    rebuildIndex ps'
    writeConfig ps'
    liftIO $ putMVar mpackages ps'

    setMessage "Package uploaded"
    redirect RootR
    return ()
  where
    getCabal Done = error "No cabal file"
    getCabal (Next entry rest)
        | reverse (take 6 $ reverse $ entryPath entry) == ".cabal" = entryContent entry
        | otherwise = getCabal rest
    getCabal (Fail s) = error $ "Invalid tarball: " ++ show s

getIndexR = do
    path <- rootDir `fmap` getYesod
    sendFile "application/x-tar" $ path ++ "/00-index.tar.gz"
    return ()

getTarballR name = do
    path <- rootDir `fmap` getYesod
    sendFile "application/x-tar" $ path ++ "/package/" ++ name
    return ()

main = do
    progname <- getProgName
    args <- cmdArgsRun $ cmdArgsMode $ Args
        { port = 3500 &= help "Port number"
        , password = Nothing &= help "Optional password required to upload files"
        , localhost = False &= help "Only allow connections from localhost?"
        , rootdir = Nothing &= help "Root folder for Yackage config file and packages"
        , title = "Yackage" &= help "Page title displayed in browser"
        } &= program progname &= summary "Run a Yackage server"
    path <-
        case rootdir args of
            Nothing -> getAppUserDataDirectory "yackage"
            Just s -> return s
    createDirectoryIfMissing True $ path ++ "/package"
    let config = path ++ "/config.yaml"
    e <- doesFileExist config
    mMay <-
        if e
            then decodeFile config
            else return $ Just Map.empty
    m <- maybe (error "Invalid Yackage config file") return mMay
    m' <- liftIO $ newMVar m
    app <- toWaiApp $ Yackage path m' (password args) (title args)
    putStrLn $ "Running Yackage on port " ++ show (port args) ++ ", rootdir: " ++ path
    runSettings
        ( setPort (port args)
        $ setHost (if localhost args then "127.0.0.1" else "*")
          defaultSettings) app

unPackageName' (PackageName s) = s

rebuildIndex ps = do
    path <- rootDir `fmap` getYesod
    let index = path ++ "/00-index.tar.gz"
    entries <- liftIO $ Tar.pack path $ cabals path
    let entries' = map (fix path) entries
    liftIO $ L.writeFile index $ compress $ Tar.write entries'
  where
    right (Right x) = x
    fix path entry = entry { Tar.entryTarPath = right $ Tar.toTarPath False $ fix' path $ Tar.fromTarPath $ Tar.entryTarPath entry }
    fix' path fp =
        if take (length path') fp == path'
            then drop (length path') fp
            else fp
      where
        path' = path ++ "/"
    cabals path = concatMap go $ Map.toList ps
    go (name, vs) = map (go' name) $ Set.toList vs
    go' name version = concat
        [ T.unpack (toPathPiece name)
        , '/' : T.unpack (toPathPiece version)
        , '/' : cabalName name version
        ]

writeConfig ps = do
    path <- rootDir `fmap` getYesod
    let config = path ++ "/config.yaml"
    liftIO $ encodeFile config $ encodeConfig ps

encodeConfig =
    object . map go . Map.toList
  where
    go (pn, vs) = toPathPiece pn .= array (map go' $ Set.toList vs)
    go' = String . toPathPiece

instance FromJSON PackageDB where
    parseJSON (Object o) = do
        fmap Map.fromList $ mapM go $ HMap.toList o
      where
        go (name, Array vs) = do
            Just name' <- return $ fromPathPiece name
            vs'' <- mapM go' $ Vector.toList vs
            return (name', Set.fromList vs'')
        go _ = fail "parseConfig.go"
        go' (String s) = do
            Just x <- return $ fromPathPiece s
            return x
        go' _ = fail "parseConfig.go'"
    parseJSON _ = fail "parseConfig"
