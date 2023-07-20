{-# LANGUAGE UndecidableInstances #-}
module User where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (MonadReader)
import DbConnPool (DbConnPool, withConnM)
import GHC.Records (HasField)
import User.Command qualified as Command
import User.Model
import User.Query qualified as Query

class Monad m => UserQuery m where
  findUserById :: UserId -> m (Maybe User)
  findUserByEmailAddress :: EmailAddress -> m (Maybe User)
  findUserByCredentials :: EmailAddress -> PasswordHash -> m (Maybe User)
  computePasswordHash :: EmailAddress -> Password -> m PasswordHash

class Monad m => UserCommand m where
  createUser :: EmailAddress -> PasswordHash -> m (Maybe User)
  updateUser :: UserId -> Maybe PasswordHash -> m (Maybe User)

instance (Monad m, MonadIO m, MonadReader r m, HasField "dbConnPool" r DbConnPool) => UserQuery m where
  findUserById userId =
    withConnM $ \conn -> Query.findUserById conn userId

  findUserByEmailAddress emailAddress =
    withConnM $ \conn -> Query.findUserByEmailAddress conn emailAddress

  findUserByCredentials emailAddress passwordHash =
    withConnM $ \conn -> Query.findUserByCredentials conn emailAddress passwordHash

  computePasswordHash emailAddress password = liftIO $ Query.computePasswordHash emailAddress password

instance (Monad m, MonadIO m, MonadReader r m, HasField "dbConnPool" r DbConnPool) => UserCommand m where
  createUser emailAddress passwordHash =
    withConnM $ \conn -> Command.createUser conn emailAddress passwordHash

  updateUser userId passwordHash =
    withConnM $ \conn -> Command.updateUser conn userId passwordHash

