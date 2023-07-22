module Web.Api.Routes.Blog (
    BlogApi, blogApiHandler
) where

import Blog
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Web.Auth
import Web.RouteHandler
import Web.Util

type BlogApi =
  "entries" :> (
       GetEntries 
  :<|> GetEntry
  :<|> CreateEntry
  :<|> UpdateEntry
  )

blogApiHandler :: RouteHandler BlogApi
blogApiHandler =
       getEntriesHandler
  :<|> getEntryHandler
  :<|> createEntryHandler
  :<|> updateEntryHandler

-------------------------------------------------------------------------------------------
-- GET /entries

data EntryDesc = EntryDesc
  { blogEntryId :: BlogEntryId
  , title :: BlogEntryTitle
  , publishDate :: PublishDate
  , isPublished :: IsPublished
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type GetEntries = Get '[JSON] [EntryDesc]

getEntriesHandler :: RouteHandler GetEntries
getEntriesHandler = do
  results <- findBlogEntries
  pure $ fmap (\BlogEntry {..} -> EntryDesc {..}) results

-------------------------------------------------------------------------------------------
-- GET /entries/:entryId

type GetEntry = Capture "entryId" BlogEntryId :> Get '[JSON] BlogEntry

getEntryHandler :: RouteHandler GetEntry
getEntryHandler blogEntryId = findBlogEntryById blogEntryId >>= orNotFound

-------------------------------------------------------------------------------------------
-- POST /entries

type CreateEntry = Auth :> Post '[JSON] BlogEntry

createEntryHandler :: RouteHandler CreateEntry
createEntryHandler userId = createBlogEntry userId >>= orThrowError err500

-------------------------------------------------------------------------------------------
-- PATCH /entries/:entryId

type UpdateEntry =
     Auth
  :> Capture "blogEntryId" BlogEntryId
  :> ReqBody '[JSON] UpdateEntryReqBody
  :> Patch '[JSON] BlogEntry

data UpdateEntryReqBody = UpdateEntryReqBody
  { title :: Maybe BlogEntryTitle
  , content :: Maybe BlogEntryContent
  , publishDate :: Maybe PublishDate
  , isPublished :: Maybe IsPublished
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

updateEntryHandler :: RouteHandler UpdateEntry
updateEntryHandler userId' blogEntryId UpdateEntryReqBody {..} = do
  BlogEntry {userId} <- findBlogEntryById blogEntryId >>= orNotFound
  when (userId /= userId') (throwError err403)
  updateBlogEntry blogEntryId title content publishDate isPublished >>= orThrowError err500

