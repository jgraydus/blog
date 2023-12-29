module File where

import Blog.Model (BlogEntryId)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import DbConnPool (HasDbConnPool, withConnM)
import File.Command qualified as Command
import File.Model
import File.Query qualified as Query

class Monad m => FileQuery m where
  findFileById :: FileId -> m (Maybe (FileEntity, FileData))

class Monad m => FileCommand m where
  createFile :: BlogEntryId -> FileName -> MimeType -> m FileEntity
  updateFileData :: FileId -> FileData -> m (Maybe ())

instance
  ( Monad m
  , MonadIO m
  , MonadReader r m
  , HasDbConnPool r
  ) => FileQuery m where

  findFileById fileId = withConnM $ \conn ->
    Query.findFileById conn fileId

instance
  ( Monad m
  , MonadIO m
  , MonadReader r m
  , HasDbConnPool r
  ) => FileCommand m where

  createFile blogEntryId name mimeType = withConnM $ \conn ->
    Command.createFile conn blogEntryId name mimeType

  updateFileData fileId fileData = withConnM $ \conn ->
    Command.updateFileData conn fileId fileData

