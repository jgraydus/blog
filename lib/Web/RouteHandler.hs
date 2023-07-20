module Web.RouteHandler where

import Control.Monad.Except (MonadError)
import Servant (ServerT, ServerError)
import User (UserCommand, UserQuery)

type RouteHandler api = forall m. Constraints m => ServerT api m

type Constraints m =
  ( Monad m
  , MonadError ServerError m
  , UserCommand m
  , UserQuery m
  )

