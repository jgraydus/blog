module Web.Application where

import Config (ApplicationConfig)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.UUID.V4 (nextRandom)
import Network.Wai
import Servant
import Web.Api
import Web.RequestContext
import Web.Site

p :: Proxy (Api :<|> Site)
p = Proxy


app :: ApplicationConfig -> Application
app config req res = do
  requestId <- nextRandom
  let reqCtx = RequestContext { requestId, config }
      ctx :: Context '[RequestContext] = reqCtx :. EmptyContext
      nt :: ReaderT RequestContext (ExceptT ServerError IO) x -> Handler x
      nt m = Handler (runReaderT m reqCtx)

  serveWithContextT p ctx nt (apiHandler :<|> siteHandler) req res

