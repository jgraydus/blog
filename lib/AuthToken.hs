{-# LANGUAGE UndecidableInstances #-}
module AuthToken where

import Config
import Control.Exception (throw)
import Control.Monad ((<=<))
import Control.Monad.Reader (asks, MonadReader)
import Data.Aeson (decode', encode, FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Text.Encoding (encodeUtf8)
import GHC.Records (getField, HasField)
import Jose.Jwa (JwsAlg(HS256))
import Jose.Jws qualified as Jws
import Jose.Jwt (Jwt(..))
import Network.HTTP.Types (hCookie)
import Network.Wai (Request, requestHeaders)
import Web.Cookie

type JwtKey = ByteString

class Monad m => AuthTokenOps m where
  makeAuthTokenCookie :: ToJSON a => a -> m SetCookie
  getAuthTokenFromRequest :: FromJSON a => Request -> m (Maybe a)

instance (Monad m, MonadReader r m, HasField "config" r ApplicationConfig) => AuthTokenOps m where
  makeAuthTokenCookie :: ToJSON a => a -> m SetCookie
  makeAuthTokenCookie val = do
    key <- asks (encodeUtf8 . jwtKey . serverConfig . getField @"config")
    case Jws.hmacEncode HS256 key (encodeStrict val) of
      Right (Jwt jwt) -> pure $ defaultSetCookie
                         { setCookieName = "authorization"
                         , setCookieValue = jwt
                         , setCookieHttpOnly = True
                         , setCookieSameSite = Just sameSiteStrict
                         }
      Left _ -> throw $ userError "failed to generate auth token for cookie"
    where
      encodeStrict = LBS.toStrict . encode
  
  getAuthTokenFromRequest :: FromJSON a => Request -> m (Maybe a)
  getAuthTokenFromRequest req = do
    key <- asks (encodeUtf8 . jwtKey . serverConfig . getField @"config")
    case getAuthCookie req of
      Nothing -> pure Nothing
      Just authCookie -> do
        case Jws.hmacDecode key authCookie of
          Left _ -> pure Nothing
          Right (_, decodedTokenBS) -> pure $ decode' (LBS.fromStrict decodedTokenBS)
    where
      getAuthCookie :: Request -> Maybe ByteString
      getAuthCookie = fmap snd . find isAuthCookie . getCookies
      isAuthCookie = (== "authorization") . fst
      getCookies = parseCookies <=< (map snd . filter isCookie . requestHeaders)
      isCookie = (== hCookie) . fst

