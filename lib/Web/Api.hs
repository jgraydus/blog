module Web.Api where

import Servant
import Web.Api.Routes.Blog
import Web.RouteHandler

type Api = "api" :> BlogApi

apiHandler :: RouteHandler Api
apiHandler = blogApiHandler

