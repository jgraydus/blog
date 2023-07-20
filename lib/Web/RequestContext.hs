module Web.RequestContext (
    makeRequestId, RequestContext(..)
) where

import Config
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import DbConnPool (DbConnPool)
import Logger

makeRequestId :: IO UUID
makeRequestId = nextRandom

data RequestContext = RequestContext
  { config :: ApplicationConfig
  , dbConnPool :: DbConnPool
  , logger :: Logger
  , requestId :: UUID
  }

