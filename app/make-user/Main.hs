module Main where

import Config
import CommandLineArgs
import Control.Monad.Reader (runReaderT)
import Data.IORef
import DbConnPool (createDbConnPool, DbConnPool, HasDbConnPool(..))
import Logger
import System.Exit (exitFailure, exitSuccess)
import User

data Ctx = Ctx
  { dbConnPool :: DbConnPool
  , config :: ApplicationConfig
  }

instance HasDbConnPool Ctx where
  getDbConnPool = dbConnPool

instance HasPasswordSalt Ctx where
  getPasswordSalt = getPasswordSalt . config

main :: IO ()
main = do
  resultRef <- newIORef False
  CommandLineArgs { configFilePath, emailAddress, password } <- parseCommandLineArgs
  configEither <- loadConfig configFilePath

  case configEither of
    Left err -> do
      withLogger INFO $ \logger -> do
        logger ERROR $ "failed to parse config file. reason:" <> toLogStr err
        writeIORef resultRef False

    Right config -> do
      let ApplicationConfig { databasePath, logLevel } = config
      withLogger logLevel $ \logger -> do
        logger INFO "creating user..."
        dbConnPool <- createDbConnPool databasePath

        userMaybe <- flip runReaderT (Ctx dbConnPool config) $ do
          passwordHash <- computePasswordHash emailAddress password
          createUser emailAddress passwordHash

        case userMaybe of
          Nothing   -> do
            logger ERROR "FAILURE! please ensure that the email address is not already used"
            writeIORef resultRef False

          Just user -> do
            logger INFO ("SUCCESS! new user: " <> toLogStr (show user))
            writeIORef resultRef True
       
  result <- readIORef resultRef
  if result then exitSuccess else exitFailure

