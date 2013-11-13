{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit
import System.Environment
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Char8 as S8

main = do
    args <- getArgs
    let (file, url, pass) =
            case args of
                [x] -> (x, "http://localhost:3500/", "")
                [x, y] -> (x, y, "")
                [x, y, z] -> (x, y, z)
                _ -> error "Usage: yackage-upload <file> [url] [password]"
    req <- parseUrl url
    body <- mkBody pass file
    let req' = req
            { method = "POST"
            , requestHeaders =
                [ ("Content-Type", "multipart/form-data; boundary=" `S8.append` bound)
                ]
            , requestBody = RequestBodyLBS body
            }
    res <- withManager $ httpLbs req'
    return ()

bound = "YACKAGEUPLOAD"

mkBody pass file = do
    file' <- L.readFile file
    let boundary = fromByteString bound
    return $ toLazyByteString $ mconcat
        [ fromByteString "--"
        , boundary
        , fromByteString "\r\nContent-Disposition: form-data; name=\"password\"\r\nContent-Type: text/plain\r\n\r\n"
        , fromString pass
        , fromByteString "\r\n--"
        , boundary
        , fromByteString "\r\nContent-Disposition: form-data; name=\"file\"; filename=\""
        , fromString file
        , fromByteString "\"\r\nContent-Type: application/x-tar\r\n\r\n"
        , fromLazyByteString file'
        , fromByteString "\r\n--"
        , boundary
        , fromByteString "--\r\n"
        ]
