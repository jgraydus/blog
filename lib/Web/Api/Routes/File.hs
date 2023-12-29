module Web.Api.Routes.File (
    FileApi, fileApiHandler
) where

import Blog.Model (BlogEntryId)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import File
import File.Model
import GHC.Generics (Generic)
import Servant
import Web.ContentTypes (Dynamic, WithContentType(..))
import Web.RouteHandler
import Web.Util (orNotFound)

type FileApi =
  "files" :> (
       CreateFile
  :<|> GetFile
  :<|> UploadFile
  )

fileApiHandler :: RouteHandler FileApi
fileApiHandler =
       createFileHandler
  :<|> getFileHandler
  :<|> uploadFileHandler

--------------------------------------------------------------------------------
-- GET /files/:fileId

type GetFile = Capture "fileId" FileId :> Get '[Dynamic] WithContentType

getFileHandler :: RouteHandler GetFile
getFileHandler fileId = do
  (FileEntity { mimeType }, content) <- findFileById fileId >>= orNotFound
  pure $ WithContentType { mimeType, content }

--------------------------------------------------------------------------------
-- POST /files

data CreateFileReqBody = CreateFileReqBody
  { blogEntryId :: BlogEntryId
  , name :: FileName
  , mimeType :: MimeType
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

type CreateFile = ReqBody '[JSON] CreateFileReqBody :> Post '[JSON] FileEntity

createFileHandler :: RouteHandler CreateFile
createFileHandler CreateFileReqBody { blogEntryId, name, mimeType } =
  createFile blogEntryId name mimeType

--------------------------------------------------------------------------------
-- PATCH /files/:fileId

type UploadFile =
     Capture "fileId" FileId
  :> ReqBody '[OctetStream] ByteString
  :> Patch '[JSON] ()

uploadFileHandler :: RouteHandler UploadFile
uploadFileHandler fileId fileData = do
  _ <- updateFileData fileId fileData >>= orNotFound
  pure ()

