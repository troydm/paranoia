{-# LANGUAGE OverloadedStrings #-}
module Proxy where

import qualified Logger as L
import qualified Data.ByteString.Char8 as B
import qualified Network.BSD as NBSD
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp (run)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.OpenSSL as ClientSSL
import Data.Time.LocalTime
import Control.Concurrent (ThreadId,forkIO)
import Control.Concurrent.Chan
import Data.ByteString.Lazy
import qualified Data.List.Split as Split
import Data.Streaming.Network
import Control.Exception (finally)
import Control.Monad (forever,when)
import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (MVar(..),takeMVar,putMVar,newEmptyMVar,isEmptyMVar)
import System.Posix.Unistd (usleep)
import Network.Socket (isConnected)
import Control.Exception (try)
import qualified OpenSSL.Session as SSL

splitHostAndPort :: String -> Maybe (String, Int)
splitHostAndPort hostport = case Split.splitOn ":" hostport of
                                [host,port] -> Just (host, read port :: Int)
                                _ -> Nothing

removeHeaders headers headersToRemove = Prelude.filter (\(h,_) -> not (inlist h headersToRemove)) headers
                                where inlist h (x:l) = h == x || inlist h l 
                                      inlist h [] = False
                    

getRequest :: Client.Manager -> Request -> String -> IO (Client.Response ByteString)
getRequest man req url = do
                        initialRequest <- Client.parseUrl url
                        requestBody <- requestBody req
                        let requestWithQS = Client.setQueryString (queryString req) initialRequest
                        let request = requestWithQS { 
                            Client.method=methodGet, 
                            Client.redirectCount=0,
                            Client.requestHeaders=(if url == paranoiaUrl then [] else (removeHeaders (requestHeaders req) [hContentLength])),  
                            Client.requestBody=Client.RequestBodyBS requestBody
                        } 
                        Client.httpLbs request man

postRequest :: Client.Manager -> Request -> String -> IO (Client.Response ByteString)
postRequest man req url = do
                        initialRequest <- Client.parseUrl url
                        requestBody <- requestBody req
                        let request = initialRequest { 
                            Client.method=methodPost, 
                            Client.redirectCount=0,
                            Client.requestHeaders=(removeHeaders (requestHeaders req) [hContentLength]),  
                            Client.requestBody = Client.RequestBodyBS requestBody 
                        }
                        Client.httpLbs request man

startLogger :: (L.Logger l) => l -> Chan (LocalTime,Request) -> IO ThreadId
startLogger l c = forkIO logData
                    where logData = do (t,r) <- readChan c
                                       L.log l t r 
                                       logData

paranoiaUrl = "http://i.imgur.com/zXMBlV0.jpg"

isLocalHost :: String -> String -> Int -> Bool
isLocalHost host hostname port = host == (hostname ++ ":" ++ (show port)) || host == ("localhost:"++(show port)) || host == hostname || host == "localhost"

buildRequestUrl :: Request -> String -> Int -> String
buildRequestUrl request hostname port = 
                        let unpackedpath = (B.unpack $ rawPathInfo request) in
                        let path = if unpackedpath == "/" then "" else unpackedpath in
                        case (requestHeaderHost request) of
                            Just u -> let uu = (B.unpack u) in 
                                      if isLocalHost uu  hostname port then paranoiaUrl else "http://"++uu++unpackedpath
                            Nothing -> paranoiaUrl

forkThread :: IO () -> IO (MVar ())
forkThread proc = do handle <- newEmptyMVar
                     _ <- forkFinally proc (\_ -> putMVar handle ())
                     return handle

foreverUntil :: IO Bool -> IO a -> IO ()
foreverUntil cond act = do tf <- cond 
                           if tf then act >> foreverUntil cond act else return ()

doUntil :: IO Bool -> IO a -> IO ()
doUntil cond act = do act
                      tf <- cond 
                      if tf then doUntil cond act else return ()

isAppDataSocketConnected appData = case appRawSocket appData of 
                                        Just s -> isConnected s
                                        Nothing -> return False

tunnel (Just host) = case (splitHostAndPort (B.unpack host)) of
                        Just (host,port) -> flip responseRaw (tunnel Nothing) $ \clientBody response -> do
                                                runTCPClient (clientSettingsTCP port (B.pack host)) $ \remoteData -> do
                                                    response "HTTP/1.1 200 Connection Established\r\nProxy-agent: paranoia\r\n\r\n"
                                                    connClosed <- newEmptyMVar
                                                    reader <- forkThread $ foreverUntil (isEmptyMVar connClosed) (do
                                                                            inp <- clientBody
                                                                            if (not $ B.null inp) then appWrite remoteData inp else usleep 100000
                                                                            )
                                                    doUntil (isAppDataSocketConnected remoteData) (do 
                                                                                            d <- appRead remoteData 
                                                                                            if (B.null d) then usleep 100000 else response d) `finally` putMVar connClosed ()
                                                    takeMVar reader
                        Nothing -> (tunnel Nothing)


tunnel Nothing = responseLBS status404 [("Content-Type", "text/plain")] "404 Not found"

fixResponseHeaders hs = ("Proxy-Agent","paranoia"):(removeHeaders hs [hContentEncoding,hContentLength])

processResponse respond req = do res <- try req 
                                 case res of 
                                    Left ex -> processResponseEx ex
                                    Right clientResponse -> respond $ responseLBS
                                                                (Client.responseStatus clientResponse)
                                                                (fixResponseHeaders (Client.responseHeaders clientResponse))
                                                                (Client.responseBody clientResponse)
                                where processResponseEx (Client.StatusCodeException s hs _) = 
                                            case lookup "X-Response-Body-Start" hs of
                                                Just body -> respond $ responseLBS s (fixResponseHeaders hs) (fromStrict body)
                                                Nothing -> respond $ responseLBS s (fixResponseHeaders hs) ""
                                      processResponseEx (Client.FailedConnectionException host port)  = do 
                                                                Prelude.putStrLn ("Timeout while connecting to " ++ host ++ ":" ++ (show port))
                                                                respond $ responseLBS status502 [("Proxy-Agent","paranoia")] "Ohh nouz something went wrong"
                                      processResponseEx (Client.FailedConnectionException2 host port secure ex)  = do 
                                                                Prelude.putStrLn ("Failure while connecting to " ++ host ++ ":" ++ (show port) ++ " secure: "++(show secure) ++ "  "++(show ex)) 
                                                                respond $ responseLBS status502 [("Proxy-Agent","paranoia")] "Ohh nouz something went wrong"
                                      processResponseEx ex = do Prelude.putStrLn (show ex) 
                                                                respond $ responseLBS status502 [("Proxy-Agent","paranoia")] "Ohh nouz something went wrong"

app :: Client.Manager -> Chan (LocalTime,Request) -> String -> Int -> Application
app man logChan hostname port = \request respond -> do
                                                t <- getZonedTime
                                                writeChan logChan (zonedTimeToLocalTime t, request)
                                                case requestMethod request of
                                                   "GET" ->  processResponse respond $ getRequest man request (buildRequestUrl request hostname port)
                                                   "POST" -> processResponse respond $ postRequest man request (buildRequestUrl request hostname port)
                                                   "CONNECT" -> respond $ tunnel (requestHeaderHost request)
                                                   _ -> respond $ responseLBS
                                                                    status405
                                                                    [("Content-Type", "text/plain")]
                                                                    "405 Method not supported" 


runProxy :: (L.Logger l) => Int -> l -> IO ()
runProxy port logger = do
                        man <- Client.newManager (ClientSSL.opensslManagerSettings SSL.context) --Client.defaultManagerSettings
                        hostname <- NBSD.getHostName
                        logChan <- newChan
                        startLogger logger logChan
                        Prelude.putStrLn $ "paranoia started on http://0.0.0.0:" ++ (show port) ++ "/"
                        run port (app man logChan hostname port)
