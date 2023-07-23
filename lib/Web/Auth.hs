module Web.Auth (
    Auth, AuthMaybe
) where

import AuthToken
import Control.Monad.Reader (runReaderT)
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import User.Model
import Web.RequestContext

-- any route which includes Auth requires that the auth token cookie is present in the request.
-- otherwise, it'll fail with a 401 response. if the cookie is present, then the encoded user's id
-- is passed to the handler.
data Auth

authorize :: RequestContext -> DelayedIO UserId
authorize reqCtx = withRequest $ \req -> do
  tokenMaybe :: Maybe User <- flip runReaderT reqCtx $ getAuthTokenFromRequest req
  case tokenMaybe of
    Just User {userId} -> pure userId
    -- no auth token!
    Nothing -> delayedFailFatal err401

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

-- routes which include AuthMaybe can be used with or without the auth token in the request.
-- if the token is present, the encoded user's id will be passed to the handler. if not, then
-- Nothing will be pased to the handler
data AuthMaybe

authorizeMaybe :: RequestContext -> DelayedIO (Maybe UserId)
authorizeMaybe reqCtx = withRequest $ \req -> do
  tokenMaybe :: Maybe User <- flip runReaderT reqCtx $ getAuthTokenFromRequest req
  pure $ userId <$> tokenMaybe

instance
  ( HasServer api ctx
  , HasContextEntry ctx RequestContext
  ) => HasServer (AuthMaybe :> api) ctx
  where
    type ServerT (AuthMaybe :> api) m = Maybe UserId -> ServerT api m

    route _ ctx s =
      let reqCtx = getContextEntry ctx
          p :: Proxy api = Proxy
      in  route p ctx (addAuthCheck s $ authorizeMaybe reqCtx)

    hoistServerWithContext _ p2 nt s =
      let p1 :: Proxy api = Proxy
      in  hoistServerWithContext p1 p2 nt . s

