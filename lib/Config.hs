module Config where

import Data.Aeson (eitherDecodeFileStrict, FromJSON)
import GHC.Generics (Generic)

data StaticAssetPaths = StaticAssetPaths
  { jsBundlePath :: FilePath
  , jsSrcMapPath :: FilePath
  , cssBundlePath :: FilePath
  , indexPath :: FilePath
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

newtype ServerConfig = ServerConfig
  { port :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data ApplicationConfig = ApplicationConfig
  { staticAssetPaths :: StaticAssetPaths
  , serverConfig :: ServerConfig
  , databasePath :: FilePath
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadConfig :: FilePath -> IO (Either String ApplicationConfig)
loadConfig = eitherDecodeFileStrict

