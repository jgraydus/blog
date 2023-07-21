module User.Model where

import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

type UserId = Int64
type EmailAddress = Text
type Password = Text
type PasswordHash = Text

data User = User
  { userId :: UserId
  , emailAddress :: EmailAddress
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

