module Web.Util where

import Control.Monad.Except (MonadError)
import Servant

orThrowError :: (Monad m, MonadError ServerError m) => ServerError -> Maybe a -> m a
orThrowError err = \case
  Just x -> pure x
  Nothing -> throwError err

orNotFound :: (Monad m, MonadError ServerError m) => Maybe a -> m a
orNotFound = orThrowError err404

