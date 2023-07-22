{-# LANGUAGE UndecidableInstances #-}
module Blog (
    BlogCommand(..), BlogQuery(..),
    module Blog.Model
) where

import Blog.Command qualified as Command
import Blog.Model
import Blog.Query qualified as Query
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import DbConnPool (DbConnPool, withConnM)
import GHC.Records (HasField)
import User.Model (UserId)

class Monad m => BlogQuery m where
  findBlogEntries :: m [BlogEntry]
  findBlogEntryById :: BlogEntryId -> m (Maybe BlogEntry)

class Monad m => BlogCommand m where
  createBlogEntry :: UserId -> m (Maybe BlogEntry)
  updateBlogEntry :: BlogEntryId -> Maybe BlogEntryTitle -> Maybe BlogEntryContent -> Maybe PublishDate -> Maybe IsPublished -> m (Maybe BlogEntry)

instance (Monad m, MonadIO m, MonadReader r m, HasField "dbConnPool" r DbConnPool) => BlogQuery m where
  findBlogEntries =
    withConnM Query.findBlogEntries
  findBlogEntryById blogEntryId =
    withConnM $ \conn -> Query.findBlogEntryById conn blogEntryId

instance (Monad m, MonadIO m, MonadReader r m, HasField "dbConnPool" r DbConnPool) => BlogCommand m where
  createBlogEntry userId =
    withConnM $ \conn -> Command.createBlogEntry conn userId
  updateBlogEntry blogEntryId title content publishDate isPublished =
    withConnM $ \conn -> Command.updateBlogEntry conn blogEntryId title content publishDate isPublished

