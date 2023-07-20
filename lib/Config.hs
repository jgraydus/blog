module Config where

import Data.Aeson (eitherDecodeFileStrict, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Logger (LogLevel)

data StaticAssetPaths = StaticAssetPaths
  { cssBundlePath :: FilePath
  , indexPath :: FilePath
  , jsBundlePath :: FilePath
  , jsSrcMapPath :: FilePath
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data ServerConfig = ServerConfig
  { jwtKey :: Text
  , port :: Int
  , staticAssetPaths :: StaticAssetPaths
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data ApplicationConfig = ApplicationConfig
  { databasePath :: FilePath
  , logLevel :: LogLevel
  , serverConfig :: ServerConfig
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadConfig :: FilePath -> IO (Either String ApplicationConfig)
loadConfig = eitherDecodeFileStrict

