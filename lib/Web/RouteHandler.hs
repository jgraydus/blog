module Web.RouteHandler where

import AuthToken
import Blog (BlogCommand, BlogQuery)
import Control.Monad.Except (MonadError)
import Servant (ServerT, ServerError)
import User (UserCommand, UserQuery)

type RouteHandler api = forall m. Constraints m => ServerT api m

type Constraints m =
  ( AuthTokenOps m
  , BlogCommand m
  , BlogQuery m
  , Monad m
  , MonadError ServerError m
  , UserCommand m
  , UserQuery m
  )

