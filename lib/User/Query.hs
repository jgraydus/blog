module User.Query (
    computePasswordHash, findUserByCredentials, findUserByEmailAddress, findUserById
) where

import Crypto.Error (throwCryptoErrorIO)
import Crypto.KDF.Argon2 qualified as Argon2
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.SQLite.Simple (Connection, Only(..), query)
import User.Model

findUserById :: Connection -> UserId -> IO (Maybe User)
findUserById conn userId = do
  result :: [Only EmailAddress] <- query conn "SELECT (email_address) FROM users WHERE user_id=?" (Only userId)
  case result of
    [] -> pure Nothing
    [Only emailAddress] -> pure . Just $ User userId emailAddress

findUserByEmailAddress :: Connection -> EmailAddress -> IO (Maybe User)
findUserByEmailAddress conn emailAddress = do
  result :: [Only UserId] <- query conn "SELECT (user_id) FROM users WHERE email_address=?" (Only emailAddress)
  case result of
    [] -> pure Nothing
    [Only userId] -> pure . Just $ User userId emailAddress

findUserByCredentials :: Connection -> EmailAddress -> PasswordHash -> IO (Maybe User)
findUserByCredentials conn emailAddress passwordHash = do
  result :: [Only UserId] <-
    query conn "SELECT (user_id) FROM users WHERE email_address=? AND password_hash=?" (emailAddress, passwordHash)
  case result of
    [] -> pure Nothing
    [Only userId] -> pure . Just $ User userId emailAddress

computePasswordHash :: EmailAddress -> Password -> IO PasswordHash
computePasswordHash emailAddress password = do
  let e = encodeUtf8 emailAddress
      p = encodeUtf8 password
      opts = Argon2.defaultOptions
      result = Argon2.hash opts p e 128
  decodeUtf8 <$> throwCryptoErrorIO result

