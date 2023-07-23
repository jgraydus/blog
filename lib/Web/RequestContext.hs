module Web.RequestContext (
    makeRequestId, RequestContext(..)
) where

import Config
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import DbConnPool (DbConnPool, HasDbConnPool(..))
import Logger

makeRequestId :: IO UUID
makeRequestId = nextRandom

data RequestContext = RequestContext
  { config :: ApplicationConfig
  , dbConnPool :: DbConnPool
  , logger :: Logger
  , requestId :: UUID
  }

instance HasDbConnPool RequestContext where
  getDbConnPool = dbConnPool
