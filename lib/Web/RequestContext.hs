module Web.RequestContext where

import Config
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Logger

makeRequestId :: IO UUID
makeRequestId = nextRandom

data RequestContext = RequestContext
  { requestId :: UUID
  , config :: ApplicationConfig
  , logger :: Logger
  }

