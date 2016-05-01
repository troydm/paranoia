{-# LANGUAGE OverloadedStrings #-}
module Logger where

import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai
import Data.Time.LocalTime
import Data.Time.Format
import System.Directory
import System.FilePath
import Network.Socket

hostIp :: SockAddr -> String
hostIp saddr = T.unpack $ head (T.splitOn (T.pack ":") (T.pack $ show saddr))

unpackJust :: Maybe B.ByteString -> B.ByteString
unpackJust s = case s of
                    Just i -> i
                    Nothing -> ""

formatLog :: String -> LocalTime -> Request -> T.Text
formatLog fmt t r = foldl' (\t (r1,r2) -> T.replace r1 r2 t) (T.pack fmt) [
    ("%ip",(T.pack (hostIp $ remoteHost r))),
    ("%headers",(T.pack (show (requestHeaders r)))),
    ("%remote",(T.pack (show $ remoteHost r))), 
    ("%path",(T.pack (show $ rawPathInfo r))), 
    ("%query",(T.pack (show $ rawQueryString r))),
    ("%host",(T.pack (show $ unpackJust (requestHeaderHost r)))),
    ("%method",(T.pack (show (requestMethod r)))),
    ("%time",(T.pack (show t))) ]

class Logger a where
    log :: a -> LocalTime -> Request -> IO ()

data FileLogger = FileLogger { path :: FilePath, logFormat :: String }

instance Logger FileLogger where
    log f t r = mkdir >> B.appendFile fullPath (TE.encodeUtf8 $ formatLog (logFormat f) t r)
                where mkdir = createDirectoryIfMissing True (takeDirectory fullPath)
                      fullPath = path f ++ "/" ++ (hostIp $ remoteHost r) ++ (formatTime defaultTimeLocale "/%Y/%m/%d.log" t)
    
