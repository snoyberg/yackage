{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
import Yesod.Core
import Yesod.Dispatch
import Yesod.Handler
import Yesod.Widget
import Yesod.Content
import Text.Hamlet
import Control.Monad.IO.Class (liftIO)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import Data.Version
import qualified Data.Map as Map
import Data.Map (Map)
import Text.ParserCombinators.ReadP
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
import Data.Object
import Data.Object.Yaml
import Control.Monad (join, unless)
import System.Console.CmdArgs
import Network.Wai
import Network.Wai.Handler.Warp (run)

data Args = Args
    { port :: Int
    , password :: Maybe String
    , localhost :: Bool
    , rootdir :: Maybe String
    } deriving (Show, Data, Typeable)

type CabalFile = FilePath
type Tarball = FilePath

data Yackage = Yackage
    { rootDir :: FilePath
    , packages :: MVar PackageDB
    , ypassword :: Maybe String
    }

type PackageDB = Map PackageName (Set Version)

mkYesod "Yackage" [$parseRoutes|
/ RootR GET POST
/00-index.tar.gz IndexR GET
/package/#String TarballR GET
|]

tarballR :: PackageName -> Version -> YackageRoute
tarballR pn v = TarballR $ tarballName pn v

tarballName pn v = concat
    [ toSinglePiece pn
    , "-"
    , toSinglePiece v
    , ".tar.gz"
    ]

cabalName pn v = concat
    [ toSinglePiece pn
    , "-"
    , toSinglePiece v
    , ".cabal"
    ]

tarballPath pn v = do
    rd <- rootDir `fmap` getYesod
    return $ concat
        [ rd
        , '/' : toSinglePiece pn
        , '/' : toSinglePiece v
        ]

instance Yesod Yackage where approot _ = ""
instance SinglePiece Version where
    fromSinglePiece s =
        case filter (\(_, y) -> null y) $ readP_to_S parseVersion s of
            [] -> Left "Invalid version"
            (x, ""):_ -> Right x
    toSinglePiece = showVersion
instance SinglePiece PackageName where
    fromSinglePiece = Right . PackageName
    toSinglePiece = unPackageName

type Handler = GHandler Yackage Yackage

getRootR :: Handler RepHtml
getRootR = do
    y <- getYesod
    ps <- getYesod >>= liftIO . readMVar . packages >>= return . Map.toList
    defaultLayout $ do
        setTitle "Yackage"
        addHamlet [$hamlet|
%h1 Yackage
%form!method=post!enctype=multipart/form-data
    %div
        Upload a new file: $
        %input!type=file!name=file
    $maybe ypassword.y _
        %div
            Password: $
            %input!type=password!name=password
    %div
        %input!type=submit!value=Upload
%dl
    $forall ps p
        %dt $unPackageName.fst.p$
        %dd
            $forall toAscList.snd.p v
                %a!href=@(tarballR.fst.p).v@ $toSinglePiece.v$
                \ $
|]

postRootR = do
    y <- getYesod
    case ypassword y of
        Nothing -> return ()
        Just p -> do
            p' <- runFormPost' $ maybeStringInput "password"
            unless (Just p == p') $ permissionDenied "Invalid password"
    rr <- getRequest
    (_, files) <- liftIO $ reqRequestBody rr
    content <-
        case lookup "file" files of
            Nothing -> error "No file upload found"
            Just fi -> return $ fileContent fi
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
    redirect RedirectTemporary RootR
    return ()
  where
    getCabal Done = error "No cabal file"
    getCabal (Next entry rest)
        | reverse (take 6 $ reverse $ entryPath entry) == ".cabal" = entryContent entry
        | otherwise = getCabal rest
    getCabal (Fail s) = error $ "Invalid tarball: " ++ s

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
        } &= program progname &= summary "Run a Yackage server"
    path <-
        case rootdir args of
            Nothing -> getAppUserDataDirectory "yackage"
            Just s -> return s
    createDirectoryIfMissing True $ path ++ "/package"
    let config = path ++ "/config.yaml"
    e <- doesFileExist config
    m <- if e
            then parseConfig `fmap` join (decodeFile config)
            else return $ Map.empty
    m' <- liftIO $ newMVar m
    app <- toWaiApp $ Yackage path m' $ password args
    let app' = if localhost args then onlyLocal app else app
    putStrLn $ "Running Yackage on port " ++ show (port args) ++ ", rootdir: " ++ path
    run (port args) app'

onlyLocal app req =
    if remoteHost req `elem` ["127.0.0.1", "::1", "localhost"]
        then app req
        else return $ Response status403 [("Content-Type", "text/plain")]
                    $ ResponseLBS "This Yackage server only talks to local clients"

unPackageName (PackageName s) = s

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
    cabals path = concatMap (go path) $ Map.toList ps
    go path (name, vs) = map (go' path name) $ Set.toList vs
    go' path name version = concat
        [ path
        , '/' : toSinglePiece name
        , '/' : toSinglePiece version
        , '/' : cabalName name version
        ]

writeConfig ps = do
    path <- rootDir `fmap` getYesod
    let config = path ++ "/config.yaml"
    liftIO $ encodeFile config $ encodeConfig ps

encodeConfig =
    Mapping . map go . Map.toList
  where
    go (pn, vs) = (toSinglePiece pn, Sequence $ map go' $ Set.toList vs)
    go' = Scalar . toSinglePiece

parseConfig :: Object String String -> PackageDB
parseConfig o = fromMaybe (error "Invalid config file") $ do
    m <- fromMapping o
    m' <- mapM go m
    return $ Map.fromList m'
  where
    go :: (String, Object String String) -> Maybe (PackageName, Set Version)
    go (name, vs) = do
        Right name' <- return $ fromSinglePiece name
        vs' <- fromSequence vs
        vs'' <- mapM go' vs'
        return (name', Set.fromList vs'')
    go' s = do
        s' <- fromScalar s
        Right x <- return $ fromSinglePiece s'
        return x
