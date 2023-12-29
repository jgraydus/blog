module Web.ContentTypes (
    CSS, Dynamic, HTML, JavaScript, WithContentType(..)
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import Servant.API.ContentTypes (Accept(..), AllCTRender(..), MimeRender(..))

------
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "Utf-8")

instance MimeRender HTML Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8

-----
data JavaScript

instance Accept JavaScript where
  contentType _ = "application" // "javascript" /: ("charset", "Utf-8")

instance MimeRender JavaScript Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8

-----
data CSS

instance Accept CSS where
  contentType _ = "text" // "css" /: ("charset", "Utf-8")

instance MimeRender CSS Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8

---------------------------------------------------------------
-- this is a bit of a hack to allow a route to dynamically set
-- its content-type header
data Dynamic

data WithContentType = WithContentType
  { mimeType :: Text
  , content :: ByteString
  }

instance Accept Dynamic where
  contentType _ = "*/*"

instance MimeRender Dynamic WithContentType where
  mimeRender _ (WithContentType { content }) = BSL.fromStrict content

instance AllCTRender '[Dynamic] WithContentType where
  handleAcceptH _ _ (WithContentType { mimeType, content }) =
    Just ( BSL.fromStrict . encodeUtf8 $ mimeType
         , BSL.fromStrict content )

