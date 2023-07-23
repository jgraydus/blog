module CommandLineArgs where

import Data.Text (Text)
import Logger (LogLevel(..))
import Options.Applicative
import Options.Applicative.Text
import Text.Read (readMaybe)

data CommandLineArgs = CommandLineArgs
  { configFilePath :: FilePath
  , emailAddress :: Text
  , password :: Text
  }
  deriving stock (Show)

parser :: Parser CommandLineArgs
parser = CommandLineArgs
  <$> strOption
      (  long "config-file-path"
      <>  short 'c'
      <>  value "./config.json"
      <>  metavar "CONFIG" )
  <*> textOption
      (  long "email-address"
      <> short 'e'
      <> metavar "EMAIL" )
  <*> textOption
      (  long "password"
      <> short 'p'
      <> metavar "PASSWORD" )

parseCommandLineArgs :: IO CommandLineArgs
parseCommandLineArgs = execParser (info parser fullDesc)

