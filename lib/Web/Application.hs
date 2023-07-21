module Web.Application where

import Config (ApplicationConfig)
import Control.Exception (catch, displayException, SomeException)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.UUID (toText)
import DbConnPool (DbConnPool)
import Logger
import Network.HTTP.Types.Status (status500)
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

      exceptionHandler :: SomeException -> IO ResponseReceived
      exceptionHandler e = do
        logger ERROR (toLogStr $ displayException e)
        res (responseLBS status500 [] "Something went wrong")

  logger INFO $ toLogStr (show req)

  result <- serveWithContextT p ctx nt (apiHandler :<|> siteHandler) req res `catch` exceptionHandler

  endTime <- getTime Monotonic
  let diff :: Double = fromIntegral $ toNanoSecs $ diffTimeSpec startTime endTime
  logger INFO $ "request took " <> toLogStr (diff / 1000000) <> " ms"

  pure result

