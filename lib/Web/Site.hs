module Web.Site (
    Site, siteHandler
)  where

import Config
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Servant
import Web.ContentTypes
import Web.RequestContext

type Index = CaptureAll "ignored" Text :> Get '[HTML] Text
type JsBundle = "bundle.js" :> Get '[JavaScript] Text
type JsSrcMap = "bundle.js.map" :> Get '[JSON] Text
type CssBundle = "bundle.css" :> Get '[CSS] Text

type Site = JsBundle :<|> JsSrcMap :<|> CssBundle :<|> Index

type Constraints r m = (Monad m, MonadIO m, MonadReader RequestContext m)

siteHandler :: Constraints r m => ServerT Site m
siteHandler = jsBundleHandler
  :<|> jsSrcMapHandler
  :<|> cssBundleHandler
  :<|> indexHandler

jsBundleHandler :: Constraints r m => ServerT JsBundle m
jsBundleHandler = do
  path <- asks (jsBundlePath . staticAssetPaths . config)
  raw <- liftIO $ BS.readFile path
  pure $ decodeUtf8 raw

jsSrcMapHandler :: Constraints r m => ServerT JsSrcMap m
jsSrcMapHandler = do
  path <- asks (jsSrcMapPath . staticAssetPaths . config)
  raw <- liftIO $ BS.readFile path
  pure $ decodeUtf8 raw

cssBundleHandler :: Constraints r m => ServerT CssBundle m
cssBundleHandler = do
  path <- asks (cssBundlePath . staticAssetPaths . config)
  raw <- liftIO $ BS.readFile path
  pure $ decodeUtf8 raw

indexHandler :: Constraints r m => ServerT Index m
indexHandler _ = do
  path <- asks (indexPath . staticAssetPaths . config)
  raw <- liftIO $ BS.readFile path
  pure $ decodeUtf8 raw
