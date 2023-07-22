module Blog.Query (
    findBlogEntryById, findBlogEntries
) where

import Blog.Model
import Database.SQLite.Simple (Connection, Only(..), query, query_)
import User.Model (UserId)

findBlogEntries :: Connection -> IO [BlogEntry]
findBlogEntries conn = do
  results :: [(BlogEntryId, BlogEntryTitle, BlogEntryContent, PublishDate, IsPublished, UserId)] <-
    query_ conn "SELECT blog_entry_id, title, content, publish_date, is_published, user_id FROM blog_entries"
  pure $ toBlogEntry <$> results
  where
    toBlogEntry (blogEntryId, title, content, publishDate, isPublished, userId) = BlogEntry {..}

findBlogEntryById :: Connection -> BlogEntryId -> IO (Maybe BlogEntry)
findBlogEntryById conn blogEntryId = do
  result :: [(BlogEntryTitle, BlogEntryContent, PublishDate, IsPublished, UserId)] <-
    query conn "SELECT title, content, publish_date, is_published, user_id FROM blog_entries WHERE blog_entry_id=?" (Only blogEntryId)
  case result of
    [(title, content, publishDate, isPublished, userId)] -> pure . Just $ BlogEntry {..}
    _ -> pure Nothing

