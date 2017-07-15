{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

newtype Response a = Response { unResponse :: a } deriving (Generic)

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

instance FromJSON (Response Account) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "account")

instance FromJSON Account where
  parseJSON (Object v) =
    Account
      <$> v .: "droplet_limit"
      <*> v .: "floating_ip_limit"
      <*> v .: "email"
      <*> v .: "uuid"
      <*> v .: "email_verified"
      <*> v .: "status"
      <*> v .: "status_message"

-- data Volume = Volume
--   { _id             :: String
--   , _region         :: Region
--   , _dropletIds     :: [Int]
--   , _name           :: String
--   , _description    :: String
--   , _sizeGigabytes  :: Int
--   , _createdAt      :: UTCTime
--   }
