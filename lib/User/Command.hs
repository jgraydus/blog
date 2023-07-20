module User.Command (
    createUser, updateUser
) where

import Data.Int (Int64)
import Database.SQLite.Simple (Connection, Only(..), query)
import User.Model

createUser :: Connection -> EmailAddress -> PasswordHash -> IO (Maybe User)
createUser conn emailAddress passwordHash = do
  [Only c] :: [Only Int64] <- query conn "SELECT COUNT(*) FROM users WHERE emailAddress=?" (Only emailAddress)
  if c == 0 then do
    [Only userId] :: [Only UserId] <-
      query conn "INSERT INTO users (email_address, password_hash) VALUES (?, ?) RETURNING user_id" (emailAddress, passwordHash)
    pure $ Just $ User userId emailAddress
  else
    pure Nothing
  

updateUser :: Connection -> UserId -> Maybe PasswordHash -> IO (Maybe User)
updateUser conn userId passwordHash = error "Command.updatePasswordHash"

