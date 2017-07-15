{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.DigitalOcean.Types where

-----------------------------------------------------------------
import GHC.Generics
import Data.Aeson
import Data.Time.Clock
import Control.Monad.State
import Control.Monad.Identity
import Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Text                 as T
import qualified Data.ByteString           as BS
-----------------------------------------------------------------
import Data.Monoid
import Data.List
-----------------------------------------------------------------

newtype QueryParams = QueryParams [(String, String)]

type Endpoint = String

data RequestMethod =
    Get
  | Post
  | Put

instance Show RequestMethod where
  show Get  = "GET"
  show Post = "POST"
  show Put  = "PUT"

instance Show QueryParams where
  show (QueryParams []) = ""
  show (QueryParams ls) = "?" <> (intercalate "&" . map (\(k, v) -> k <> "=" <> v) $ ls)

class (FromJSON a, Show a, FromJSON (PaginationState a)) => Paginatable a where

data Paginatable a => PaginationState a = PaginationState
  { curr     :: [a]
  , page     :: Int
  , nextUrl  :: Maybe String
  , total    :: Int
  , isLast   :: Bool
  } deriving (Show)

data PaginationConfig = PaginationConfig
  { pageSize :: Int
  , resultLimit :: Int
  }

newtype DO a = DO { runDO :: ReaderT Client (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader Client)

newtype Client = Client { apiKey :: BS.ByteString }

type DoErr = T.Text

-----------------------------------------------------------------

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

-----------------------------------------------------------------

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

instance FromJSON (PaginationState Action) where
  parseJSON (Object v) = do
    actions <- v .: "actions"
    links <- v .: "links"
    -- meta <- v .: "meta"
    pages <- links .: "pages"
    next <- pages .:? "next"
    total <- v .: "meta" >>= (.: "total")
    let page = 1
    return $ PaginationState actions page next total False

instance Paginatable Action where

-----------------------------------------------------------------
