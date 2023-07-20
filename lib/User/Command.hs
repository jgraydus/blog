module User.Command (
    createUser, updateUser
) where

import Database.SQLite.Simple (Connection)
import User.Model

createUser :: Connection -> EmailAddress -> PasswordHash -> IO (Maybe User)
createUser conn emailAddress passwordHash = error "Command.createUser"

updateUser :: Connection -> UserId -> Maybe PasswordHash -> IO (Maybe User)
updateUser conn userId passwordHash = error "Command.updatePasswordHash"

