module User.Query (
    computePasswordHash, findUserByCredentials, findUserById
) where

import Crypto.Error (throwCryptoErrorIO)
import Crypto.KDF.Argon2 qualified as Argon2
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.SQLite.Simple (Connection)
import User.Model

findUserById :: Connection -> UserId -> IO (Maybe User)
findUserById conn userId = error "Query.findUserById"

findUserByCredentials :: Connection -> EmailAddress -> PasswordHash -> IO (Maybe User)
findUserByCredentials conn emailAddress passwordHash = error "Query.findUserByCredentials"

computePasswordHash :: EmailAddress -> Password -> IO PasswordHash
computePasswordHash emailAddress password = do
  let e = encodeUtf8 emailAddress
      p = encodeUtf8 password
      opts = Argon2.defaultOptions
      result = Argon2.hash opts p e 128
  decodeUtf8 <$> throwCryptoErrorIO result

