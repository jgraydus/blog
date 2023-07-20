module Web.Api where

import Servant
import Web.Api.Routes.Auth
import Web.Api.Routes.Blog
import Web.Api.Routes.User
import Web.RouteHandler

type Api = "api" :> (AuthApi :<|> BlogApi :<|> UserApi)

apiHandler :: RouteHandler Api
apiHandler = authApiHandler :<|> blogApiHandler :<|> userApiHandler

