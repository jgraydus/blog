module CommandLineArgs where

import Data.Text (Text)
import Logger (LogLevel(..))
import Options.Applicative
import Options.Applicative.Text
import Text.Read (readMaybe)

data CommandLineArgs = CommandLineArgs
  { databaseFilePath :: FilePath
  , emailAddress :: Text
  , password :: Text
  , logLevel :: LogLevel
  }
  deriving stock (Show)

logLevelOption :: Mod OptionFields LogLevel -> Parser LogLevel
logLevelOption = option (maybeReader readMaybe)

parser :: Parser CommandLineArgs
parser = CommandLineArgs
  <$> strOption
      (  long "database-path"
      <> short 'd'
      <> value "./blog.db"
      <> metavar "DATABASE" )
  <*> textOption
      (  long "email-address"
      <> short 'e'
      <> metavar "EMAIL" )
  <*> textOption
      (  long "password"
      <> short 'p'
      <> metavar "PASSWORD" )
  <*> logLevelOption
      (  long "log-level"
      <> short 'l'
      <> value INFO
      <> metavar "LOGLEVEL" )

parseCommandLineArgs :: IO CommandLineArgs
parseCommandLineArgs = execParser (info parser fullDesc)

