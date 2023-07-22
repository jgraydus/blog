module Web.Api.Routes.Auth (
    AuthApi, authApiHandler
) where

import AuthToken
import Data.Aeson (FromJSON)
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import Servant
import User
import Web.Auth
import Web.Cookie
import Web.RouteHandler
import Web.Util

type AuthApi = LogIn :<|> Me :<|> SignUp :<|> LogOut

authApiHandler :: RouteHandler AuthApi
authApiHandler = logInHandler :<|> meHandler :<|> signUpHandler :<|> logOutHandler

-------------------------------------------------------------------------------------------
-- POST /login

type LogIn =
     "login"
  :> ReqBody '[JSON] LogInReqBody
  :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] User)

data LogInReqBody = LogInReqBody
  { emailAddress :: EmailAddress
  , password :: Password
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

logInHandler :: RouteHandler LogIn
logInHandler LogInReqBody { emailAddress, password } = do
  passwordHash <- computePasswordHash emailAddress password
  userMaybe <- findUserByCredentials emailAddress passwordHash
  case userMaybe of
    Nothing -> throwError err403
    Just user -> do
      cookie <- makeAuthTokenCookie user
      pure $ addHeader cookie user

-------------------------------------------------------------------------------------------
-- POST /mea

type Me = Auth :> "me" :> Post '[JSON] User

meHandler :: RouteHandler Me
meHandler userId = findUserById userId >>= orNotFound

-------------------------------------------------------------------------------------------
-- POST /signup

type SignUp =
     "signup"
  :> ReqBody '[JSON] SignUpReqBody
  :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] User)

data SignUpReqBody = SignUpReqBody
  { emailAddress :: EmailAddress
  , password :: Password
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

signUpHandler :: RouteHandler SignUp
signUpHandler SignUpReqBody { emailAddress, password } = do
  passwordHash <- computePasswordHash emailAddress password
  userMaybe <- createUser emailAddress passwordHash
  case userMaybe of
    Nothing -> throwError err403
    Just user -> do
      cookie <- makeAuthTokenCookie user
      pure $ addHeader cookie user

-------------------------------------------------------------------------------------------
-- POST /logout

type LogOut =
       "logout"
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ())

expiration :: UTCTime
expiration = fromJust $ iso8601ParseM "1970-01-01T00:00:00.000Z"

logOutHandler :: RouteHandler LogOut
logOutHandler = pure $ addHeader cookie ()
  where
    cookie :: SetCookie
    cookie = defaultSetCookie
             { setCookieName = "authorization"
             , setCookieValue = ""
             , setCookieHttpOnly = True
             , setCookieSameSite = Just sameSiteStrict
             , setCookieExpires = Just expiration
             -- TODO , setCookieSecure = True
             }

