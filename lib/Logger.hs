{-# LANGUAGE UndecidableInstances #-}
module Logger (
    Logger, LogLevel(..), LogStr, MonadLogger(..), toLogStr, withLogger
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import GHC.Records (getField, HasField)
import Prelude hiding (log)
import System.Log.FastLogger

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving stock (Generic, Eq, Ord, Read, Show)
  deriving anyclass (FromJSON)

instance ToLogStr LogLevel where
  toLogStr = \case
    TRACE -> "TRACE"
    DEBUG -> "DEBUG"
    WARN -> "WARN"
    INFO -> "INFO"
    ERROR -> "ERROR"

class (Monad m, MonadIO m) => MonadLogger m where
  log :: LogLevel -> LogStr -> m ()

type Logger = LogLevel -> LogStr -> IO ()

instance (Monad m, MonadIO m, MonadReader r m, HasField "logger" r Logger) => MonadLogger m where
  log level msg = do
    logger <- asks $ getField @"logger"
    liftIO $ logger level msg

withLogger :: LogLevel -> (Logger -> IO a) -> IO a
withLogger thresholdLevel action = do
  timeCache <- newTimeCache simpleTimeFormat'
  (doLog, cleanup) <- newTimedFastLogger timeCache (LogStdout 4096)
  let logger logLevel msg = when (logLevel >= thresholdLevel) $
        doLog (\time -> "[" <> toLogStr logLevel <> "][" <> toLogStr time <> "]" <> msg <> "\n")
  result <- action logger
  _ <- cleanup
  pure result

