module File.Model where

import Blog.Model (BlogEntryId)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

type FileId = Int64
type FileName = Text
type MimeType = Text
type FileData = ByteString

data FileEntity = FileEntity
  { fileId :: FileId
  , blogEntryId :: BlogEntryId
  , name :: FileName
  , mimeType :: MimeType
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

