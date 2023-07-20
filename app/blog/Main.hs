module Main where

import CommandLineArgs (CommandLineArgs(..), parseCommandLineArgs)
import Config (ApplicationConfig(..), loadConfig, ServerConfig(..))
import DbConnPool (createDbConnPool)
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

    Right config@ApplicationConfig { databasePath, logLevel, serverConfig } -> do
      let ServerConfig { port } = serverConfig
      withLogger logLevel $ \logger -> do
        dbConnPool <- createDbConnPool databasePath

        logger DEBUG (toLogStr . show $ config)
        logger INFO $ "server listening on port " <> toLogStr port

        Warp.run port (app config logger dbConnPool)

