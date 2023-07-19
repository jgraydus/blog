module Web.RequestContext where

import Data.UUID (UUID)
import Data.Text (Text)

data RequestContext = RequestContext
  { requestId :: UUID
  , dummy :: Text
  }

