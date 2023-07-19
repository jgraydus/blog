module Web.Api.Routes.Blog (
    BlogApi, blogApiHandler
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Web.RouteHandler

type EntryId = Int64

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
  { entryId :: EntryId
  , title :: Text
  , date :: UTCTime
  , isPublished :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type GetEntries = Get '[JSON] [EntryDesc]

getEntriesHandler :: RouteHandler GetEntries
getEntriesHandler = pure []

-------------------------------------------------------------------------------------------
-- GET /entries/:entryId

data Entry = Entry
  { entryId :: EntryId
  , title :: Text
  , date :: UTCTime
  , contents :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type GetEntry = Capture "entryId" EntryId :> Get '[JSON] Entry

getEntryHandler :: RouteHandler GetEntry
getEntryHandler = undefined

-------------------------------------------------------------------------------------------
-- POST /entries

type CreateEntry = ReqBody '[JSON] CreateEntryReqBody :> Post '[JSON] EntryId

data CreateEntryReqBody = CreateEntryReqBody
  { title :: Text
  , contents :: Text
  , date :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

createEntryHandler :: RouteHandler CreateEntry
createEntryHandler = undefined

-------------------------------------------------------------------------------------------
-- PATCH /entries/:entryId

type UpdateEntry = ReqBody '[JSON] UpdateEntryReqBody :> Patch '[JSON] ()

data UpdateEntryReqBody = UpdateEntryReqBody
  { entryId :: EntryId
  , title :: Maybe Text
  , contents :: Maybe Text
  , date :: Maybe UTCTime
  , isPublished :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

updateEntryHandler :: RouteHandler UpdateEntry
updateEntryHandler = undefined

