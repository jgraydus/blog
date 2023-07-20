module Web.Api.Routes.Auth (
    AuthApi, authApiHandler
) where

import AuthToken
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Servant
import User
import User.Model
import Web.Cookie (SetCookie)
import Web.RouteHandler

type AuthApi = LogIn

authApiHandler :: RouteHandler AuthApi
authApiHandler = logInHandler

-------------------------------------------------------------------------------------------
-- POST /login

type LogIn =
     ReqBody '[JSON] LogInReqBody
  :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] User)

data LogInReqBody = LogInReqBody
  { emailAddress :: EmailAddress
  , password :: Password
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

logInHandler :: RouteHandler LogIn
logInHandler LogInReqBody {..} = do
  passwordHash <- computePasswordHash emailAddress password
  userMaybe <- findUserByCredentials emailAddress passwordHash
  case userMaybe of
    Nothing -> throwError err403
    Just user -> do
      cookie <- makeAuthTokenCookie user
      pure $ addHeader cookie user

