CREATE TABLE users (
  user_id         INTEGER PRIMARY KEY,
  email_address   TEXT NOT NULL,
  password_hash   BLOB
);

CREATE TABLE blog_entries (
  blog_entry_id   INTEGER PRIMARY KEY,
  user_id         INTEGER NOT NULL,
  title           TEXT NOT NULL,
  content         TEXT NOT NULL,
  publish_date    TIMESTAMP WITH TIME ZONE NOT NULL,
  is_published    BOOLEAN NOT NULL
);

