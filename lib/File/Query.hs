module File.Query where

import Blog.Model (BlogEntryId)
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple (Connection, query, Only(..))
import File.Model

findFileById :: Connection -> FileId -> IO (Maybe (FileEntity, FileData))
findFileById conn fileId = do
  result :: [(BlogEntryId, FileName, MimeType, Maybe FileData)] <-
    query conn "SELECT blog_entry_id, name, mime_type, data \
               \FROM files \
               \WHERE file_id = ?"
               (Only fileId)
  case result of
    [] -> pure Nothing
    [(blogEntryId, name, mimeType, fileData)] ->
      pure $ Just ( FileEntity { fileId, blogEntryId, name, mimeType }
                  , fromMaybe "" fileData )
    _ -> error "multiple results in findFileById"

