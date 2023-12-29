module File.Command where

import Blog.Model (BlogEntryId)
import Database.SQLite.Simple
  (changes, Connection, execute, Only(..), query)
import File.Model

createFile
  :: Connection
  -> BlogEntryId
  -> FileName
  -> MimeType
  -> IO FileEntity
createFile conn blogEntryId name mimeType = do
  [Only fileId] :: [Only FileId] <-
    query conn "INSERT INTO files (blog_entry_id, name, mime_type) \
               \VALUES (?, ?, ?) \
               \RETURNING file_id"
               (blogEntryId, name, mimeType)
  pure $ FileEntity { fileId, blogEntryId, name, mimeType }

updateFileData
  :: Connection
  -> FileId
  -> FileData
  -> IO (Maybe ())
updateFileData conn fileId fileData = do
  _ <- execute conn "UPDATE files \
                    \SET data = ? \
                    \WHERE file_id = ?"
                    (fileData, fileId)
  count <- changes conn
  pure $ if count == 0 then Nothing else Just ()

