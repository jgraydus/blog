module Web.Application where

import Config (ApplicationConfig)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.UUID (toText)
import DbConnPool (DbConnPool)
import Logger
import Network.Wai
import Servant
import System.Clock
import Web.Api
import Web.RequestContext (makeRequestId, RequestContext(..))
import Web.Site

p :: Proxy (Api :<|> Site)
p = Proxy

app :: ApplicationConfig -> Logger -> DbConnPool -> Application
app config logger' dbConnPool req res = do
  startTime <- getTime Monotonic

  requestId <- makeRequestId
  let logger logLevel msg = logger' logLevel ("[requestId=" <> toLogStr (toText requestId) <> "]" <> msg)
      reqCtx = RequestContext { config, dbConnPool, logger, requestId }
      ctx :: Context '[RequestContext] = reqCtx :. EmptyContext
      nt :: ReaderT RequestContext (ExceptT ServerError IO) x -> Handler x
      nt m = Handler (runReaderT m reqCtx)

  logger INFO $ toLogStr (show req)

  result <- serveWithContextT p ctx nt (apiHandler :<|> siteHandler) req res

  endTime <- getTime Monotonic
  let diff :: Double = fromIntegral $ toNanoSecs $ diffTimeSpec startTime endTime
  logger INFO $ "request took " <> toLogStr (diff / 1000000) <> " ms"

  pure result

