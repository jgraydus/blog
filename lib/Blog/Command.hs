module Blog.Command (
    createBlogEntry, updateBlogEntry
) where

import Blog.Model
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, execute, Only(..), query, withTransaction)
import User.Model (UserId)

toBlogEntry :: (BlogEntryId, UserId, BlogEntryTitle, BlogEntryContent, PublishDate, IsPublished) -> BlogEntry
toBlogEntry (blogEntryId, userId, title, content, publishDate, isPublished) = BlogEntry {..}

createBlogEntry :: Connection -> UserId -> IO (Maybe BlogEntry)
createBlogEntry conn userId = do
  ts <- getCurrentTime
  result :: [(BlogEntryId, UserId, BlogEntryTitle, BlogEntryContent, PublishDate, IsPublished)] <-
    query conn "INSERT INTO blog_entries (user_id, title, content, publish_date, is_published) \
               \VALUES (?, ?, ?, ?, ?) \
               \RETURNING blog_entry_id, user_id, title, content, publish_date, is_published"
               (userId, "" :: Text, "" :: Text, iso8601Show ts, False)
  pure $ toBlogEntry <$> listToMaybe result

updateBlogEntry
  :: Connection
  -> BlogEntryId
  -> Maybe BlogEntryTitle
  -> Maybe BlogEntryContent
  -> Maybe PublishDate
  -> Maybe IsPublished
  -> IO (Maybe BlogEntry)
updateBlogEntry conn blogEntryId title content publishDate isPublished = withTransaction conn $ do
  for_ title       $ \val -> execute conn "UPDATE blog_entries SET title=?   WHERE blog_entry_id=?" (val, blogEntryId)
  for_ content     $ \val -> execute conn "UPDATE blog_entries SET content=? WHERE blog_entry_id=?" (val, blogEntryId)
  for_ publishDate $ \val -> execute conn "UPDATE blog_entries SET publish_date=? WHERE blog_entry_id=?" (val, blogEntryId)
  for_ isPublished $ \val -> execute conn "UPDATE blog_entries SET is_published=? WHERE blog_entry_id=?" (val, blogEntryId)

  entry :: [(BlogEntryId, UserId, BlogEntryTitle, BlogEntryContent, PublishDate, IsPublished)] <-
    query conn "SELECT blog_entry_id, user_id, title, content, publish_date, is_published \
               \FROM blog_entries \
               \WHERE blog_entry_id=?"
               (Only blogEntryId)
  pure $ toBlogEntry <$> listToMaybe entry

