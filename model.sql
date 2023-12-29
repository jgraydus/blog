CREATE TABLE IF NOT EXISTS users (
  user_id         INTEGER PRIMARY KEY,
  email_address   TEXT NOT NULL,
  password_hash   TEXT
);

CREATE TABLE IF NOT EXISTS blog_entries (
  blog_entry_id   INTEGER PRIMARY KEY,
  user_id         INTEGER NOT NULL,
  title           TEXT NOT NULL,
  content         TEXT NOT NULL,
  publish_date    TIMESTAMP WITH TIME ZONE NOT NULL,
  is_published    BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS files (
  file_id         INTEGER PRIMARY KEY,
  blog_entry_id   INTEGER NOT NULL,
  name            TEXT NOT NULL,
  mime_type       TEXT NOT NULL,
  data            BLOB
);

