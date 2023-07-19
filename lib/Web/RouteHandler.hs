module Web.RouteHandler where

import Control.Monad.Except (MonadError)
import Servant (ServerT, ServerError)

type RouteHandler api = forall m. Constraints m => ServerT api m

type Constraints m =
  ( Monad m
  , MonadError ServerError m
  )

