module Blog.Model where

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
  , publishDate :: PublishDate
  , isPublished :: IsPublished
  , userId :: UserId
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

