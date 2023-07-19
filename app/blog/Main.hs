module Main where

import CommandLineArgs (CommandLineArgs(..), parseCommandLineArgs)
import Config (ApplicationConfig(..), loadConfig, ServerConfig(..))
import Logger
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (exitFailure)
import Web.Application (app)

main :: IO ()
main = do
  CommandLineArgs {..} <- parseCommandLineArgs
  configEither <- loadConfig configFilePath

  case configEither of
    Left err -> do
      putStrLn "failed to parse config file. reason:"
      putStrLn err
      exitFailure

    Right config@ApplicationConfig { logLevel, serverConfig } -> do
      let ServerConfig { port } = serverConfig
      withLogger logLevel $ \logger -> do
        logger DEBUG (toLogStr . show $ config)
        logger INFO $ "server listenting on port " <> toLogStr port

        Warp.run port (app logger config)

