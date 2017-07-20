{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.DigitalOcean.Services.Action where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        Data.Time.Clock
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
-----------------------------------------------------------------

data Action = Action
  { actionId            :: ActionId
  -- , _status        :: String -- TODO: Make a type
  , actionType'         :: String
  , actionStartedAt     :: UTCTime
  , actionCompletedAt   :: UTCTime
  , actionResourceId    :: Int
  , actionResourceType  :: String -- TODO: Make a type
  , actionRegionSlug    :: Maybe String
  } deriving (Show, Generic)

instance FromJSON (Response Action) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "action")

instance FromJSON (Response [Action]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "actions")

instance FromJSON Action where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (PaginationState Action) where
  parseJSON (Object v) = do
    actions <- v .: "actions"
    (next, total) <- parsePagination v
    let page = 1
    return $ PaginationState actions page next total False

instance Paginatable Action where

type ActionId = Int
