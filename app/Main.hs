module Main where

import Logger (FileLogger(..))
import Proxy
import Control.Applicative
import Options.Applicative
import System.Posix.Daemonize (daemonize)



data ProxyConfig = ProxyConfig {
    port :: Int,
    logPath :: String,
    logFormat :: String,
    daemon :: Bool
}

proxyConfigParser :: Parser ProxyConfig
proxyConfigParser = ProxyConfig
        <$> option auto
        ( long "port" <> short 'p'
          <> metavar "PORT" <> value 8080
          <> help "proxy port" )
        <*> strOption
        ( long "log-path" <> short 'l'
          <> metavar "PATH" <> value "./log"
          <> help "log directory path" )
        <*> strOption
        ( long "log-format" <> short 'f'
          <> metavar "FORMAT" <> value "%time %remote %method %host %path %query\n%headers\n"
          <> help "log format like '%time %remote %method %host %path %query %headers'" )
        <*> switch
        ( long "daemon" <> short 'd'
          <> help "run as daemon" )


execProxy config = runProxy (port config) (FileLogger (logPath config) (Main.logFormat config))
exe config = if (daemon config) then daemonize $ execProxy config else execProxy config

main :: IO ()
main = execParser opts >>= exe
          where
              opts = info (helper <*> proxyConfigParser)
                            ( fullDesc
                              <> progDesc "HTTP Proxy server using Warp and http-client"
                              <> header "paranoia - a HTTP Proxy server" )
