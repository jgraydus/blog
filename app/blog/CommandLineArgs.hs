module CommandLineArgs where

import Options.Applicative

newtype CommandLineArgs = CommandLineArgs
  { configFilePath :: FilePath
  }
  deriving stock (Show)

parser :: Parser CommandLineArgs
parser = CommandLineArgs
  <$> strOption
      ( long "config-file-path"
      <> short 'c'
      <> value "./config.json"
      <> metavar "FILEPATH" )

parseCommandLineArgs :: IO CommandLineArgs
parseCommandLineArgs = execParser (info parser fullDesc)

