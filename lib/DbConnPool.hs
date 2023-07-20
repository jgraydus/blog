module DbConnPool (
   createDbConnPool, DbConnPool, withConn, withConnM
) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import GHC.Records (getField, HasField)
import Data.Pool (defaultPoolConfig, newPool, Pool, setNumStripes, withResource)
import Database.SQLite.Simple (close, Connection, open)

newtype DbConnPool = DbConnPool (Pool Connection)

createDbConnPool :: FilePath -> IO DbConnPool
createDbConnPool dbFilePath = do
  n <- getNumCapabilities
  let createResource = open dbFilePath
      freeResource = close
      poolCacheTtl = 10
      poolMaxResources = n * 4
      config = setNumStripes (Just n) $ defaultPoolConfig createResource freeResource poolCacheTtl poolMaxResources
  pool <- newPool config
  pure $ DbConnPool pool

withConn :: DbConnPool -> (Connection -> IO a) -> IO a
withConn (DbConnPool pool) = withResource pool

withConnM :: (Monad m, MonadIO m, MonadReader r m, HasField "dbConnPool" r DbConnPool) => (Connection -> IO a) -> m a
withConnM action = do
  pool <- asks (getField @"dbConnPool")
  liftIO $ withConn pool action

