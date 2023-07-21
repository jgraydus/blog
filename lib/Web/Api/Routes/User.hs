module Web.Api.Routes.User (
    UserApi, userApiHandler
) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Servant
import User
import Web.RouteHandler

type UserApi =
  "users" :> (
         CreateUser
    :<|> UpdateUser
  )

userApiHandler :: RouteHandler UserApi
userApiHandler = createUserHandler :<|> updateUserHandler

-------------------------------------------------------------------------------------------
-- POST /user

type CreateUser = ReqBody '[JSON] CreateUserReqBody :> Post '[JSON] (Maybe User)

data CreateUserReqBody = CreateUserReqBody
  { emailAddress :: EmailAddress
  , password :: Password
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

createUserHandler :: RouteHandler CreateUser
createUserHandler CreateUserReqBody {..} = do
  passwordHash <- computePasswordHash emailAddress password
  createUser emailAddress passwordHash

-------------------------------------------------------------------------------------------
-- PATCH /user/:userId

type UpdateUser = Capture "userId" UserId :> ReqBody '[JSON] UpdateUserReqBody :> Patch '[JSON] (Maybe User)

data UpdateUserReqBody = UpdateUserReqBody
  { password :: Password
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

updateUserHandler :: RouteHandler UpdateUser
updateUserHandler userId UpdateUserReqBody {..} = do
  userMaybe <- findUserById userId
  case userMaybe of
    Just (User { emailAddress }) -> do
      passwordHash <- computePasswordHash emailAddress password
      updateUser userId (Just passwordHash)
    Nothing -> pure Nothing

