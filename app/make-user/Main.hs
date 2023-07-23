module Main where

import CommandLineArgs
import Control.Monad.Reader (runReaderT)
import Data.IORef
import DbConnPool (createDbConnPool)
import Logger
import System.Exit (exitFailure, exitSuccess)
import User

main :: IO ()
main = do
  CommandLineArgs { databaseFilePath, emailAddress, password, logLevel } <- parseCommandLineArgs

  resultRef <- newIORef False

  withLogger logLevel $ \logger -> do
    logger INFO "creating user..."

    dbConnPool <- createDbConnPool databaseFilePath

    userMaybe <- flip runReaderT dbConnPool $ do
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

