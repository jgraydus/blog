module Config where

import Data.Aeson (eitherDecodeFileStrict, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Logger (LogLevel)
import User.Model

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
  , passwordSalt :: PasswordSalt
  , serverConfig :: ServerConfig
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadConfig :: FilePath -> IO (Either String ApplicationConfig)
loadConfig = eitherDecodeFileStrict

class HasPasswordSalt a where
  getPasswordSalt :: a -> PasswordSalt

instance HasPasswordSalt ApplicationConfig where
  getPasswordSalt = passwordSalt

