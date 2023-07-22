module Web.Auth where

import AuthToken
import Control.Monad.Reader (runReaderT)
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import User.Model
import Web.RequestContext

data Auth

instance 
  ( HasServer api ctx
  , HasContextEntry ctx RequestContext
  ) => HasServer (Auth :> api) ctx
  where
    type ServerT (Auth :> api) m = UserId -> ServerT api m

    route _ ctx s =
      let reqCtx = getContextEntry ctx
          p :: Proxy api = Proxy
      in  route p ctx (addAuthCheck s $ authorize reqCtx)

    hoistServerWithContext _ p2 nt s =
      let p1 :: Proxy api = Proxy
      in  hoistServerWithContext p1 p2 nt . s

authorize :: RequestContext -> DelayedIO UserId
authorize reqCtx = withRequest $ \req -> do
  tokenMaybe :: Maybe User <- flip runReaderT reqCtx $ getAuthTokenFromRequest req
  case tokenMaybe of
    Just User {userId} -> pure userId
    -- no auth token!
    Nothing -> delayedFailFatal err401

