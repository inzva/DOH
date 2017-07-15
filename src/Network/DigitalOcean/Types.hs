{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.DigitalOcean.Types where

import GHC.Generics
import Data.Aeson
import Data.Time.Clock
import Control.Monad.State
import Control.Monad.Identity
import Control.Lens

data Account = Account
  { _dropletLimit    :: Int
  , _floatingIpLimit :: Int
  , _email           :: String
  , _uuid            :: String
  , _emailVerified   :: Bool
  , _status          :: String
  , _statusMessage   :: String
  } deriving (Show, Generic)

instance FromJSON Account where
  parseJSON (Object v) = do
    v' <- v .: "account"
    Account
      <$> v' .: "droplet_limit"
      <*> v' .: "floating_ip_limit"
      <*> v' .: "email"
      <*> v' .: "uuid"
      <*> v' .: "email_verified"
      <*> v' .: "status"
      <*> v' .: "status_message"

data Action = Action
  { _id            :: Int
  -- , _status        :: String -- TODO: Make a type
  , _type'         :: String
  , _startedAt     :: UTCTime
  , _completedAt   :: UTCTime
  , _resourceId    :: Int
  , _resourceType  :: String -- TODO: Make a type
  , _regionSlug    :: String
  } deriving (Show)

class FromJSON a => Paginatable a where

instance Paginatable Action where

data Paginatable a => PaginationState a = PaginationState
  { curr :: [a]
  , nextUrl :: String
  , lastUrl :: String
  } deriving (Show)

instance FromJSON (PaginationState Action) where
  parseJSON (Object v) = do
    actions <- v .: "actions"
    links <- v .: "links"
    -- meta <- v .: "meta"
    pages <- links .: "pages"
    next <- pages .: "next"
    last <- pages .: "last"
    return $ PaginationState actions next last

instance FromJSON Action where
  parseJSON (Object v) =
    Action
      <$> v .: "id"
      -- <*> v .: "status"
      <*> v .: "type"
      <*> v .: "started_at"
      <*> v .: "completed_at"
      <*> v .: "resource_id"
      <*> v .: "resource_type"
      <*> v .: "region_slug"
