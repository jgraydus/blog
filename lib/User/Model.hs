module User.Model where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Text (Text)

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

