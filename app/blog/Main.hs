module Main where

import CommandLineArgs (CommandLineArgs(..), parseCommandLineArgs)
import Config (ApplicationConfig(..), loadConfig, ServerConfig(..))
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

    Right config@ApplicationConfig {..} -> do
      let ServerConfig { port } = serverConfig
      print config

      putStrLn $ "server listenting on port " <> show port
      Warp.run port (app config)

