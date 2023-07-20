module Blog where

import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import User.Model (UserId)

type BlogEntryId = Int64
type BlogEntryTitle = Text
type BlogEntryContent = Text
type PublishDate = UTCTime
type IsPublished = Bool

data BlogEntry = BlogEntry
  { blogEntryId :: BlogEntryId
  , title :: BlogEntryTitle
  , content :: BlogEntryContent
  , date :: PublishDate
  , isPublished :: IsPublished
  , userId :: UserId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

class Monad m => BlogQuery m where
  findBlogEntryById :: BlogEntryId -> m (Maybe BlogEntry)
  findBlogEntriesByUserId :: UserId -> m [BlogEntry]

class Monad m => BlogCommand m where
  createBlogEntry :: UserId -> BlogEntryTitle -> BlogEntryContent -> m (Maybe BlogEntry)
  updateBlogEntry :: BlogEntryId -> Maybe BlogEntryTitle -> Maybe BlogEntryContent -> Maybe PublishDate -> Maybe IsPublished -> m (Maybe BlogEntry)

