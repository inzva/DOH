{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.DigitalOcean.Types where

-----------------------------------------------------------------
import           GHC.Generics
import           Data.Aeson
import           Data.Time.Clock
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Text                 as T
import qualified Data.ByteString           as BS
import           System.FilePath.Posix     ((</>))
-----------------------------------------------------------------
import           Data.Monoid
import           Data.List
-----------------------------------------------------------------

newtype QueryParams = QueryParams [(String, String)]

data RequestMethod =
    Get
  | Post
  | Put
  | Delete

instance Show RequestMethod where
  show Get  = "GET"
  show Post = "POST"
  show Put  = "PUT"
  show Delete  = "DELETE"

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

newtype Response a = Response { unResponse :: a } deriving (Generic, Show)

class (ToJSON a) => Payload a 

data EmptyPayload = EmptyPayload
instance Payload EmptyPayload where
instance ToJSON EmptyPayload where
  toJSON EmptyPayload = object []

data Endpoint =
    AccountEndpoint
  | ActionsEndpoint
  | ActionEndpoint ActionId
  | RegionsEndpoint
  | VolumesEndpoint
  | VolumeEndpoint VolumeId
  | SnapshotsEndpoint
  | SnapshotEndpoint SnapshotId
  | VolumeSnapshotsEndpoint VolumeId
  | VolumesActionsEndpoint
  | VolumeActionEndpoint VolumeId ActionId
  | VolumeActionsEndpoint VolumeId
  | CertificateEndpoint CertificateId
  | CertificatesEndpoint
  | DomainsEndpoint
  | DomainEndpoint DomainName

instance Show Endpoint where
  show AccountEndpoint                 = "/account"
  show ActionsEndpoint                 = "/actions"
  show RegionsEndpoint                 = "/regions"
  show VolumesEndpoint                 = "/volumes"
  show SnapshotsEndpoint               = "/snapshots"
  show CertificatesEndpoint            = "/certificates"
  show DomainsEndpoint                 = "/domains"
  show (ActionEndpoint id')            = show ActionsEndpoint </> show id'
  show (VolumeEndpoint id')            = show VolumesEndpoint </> id'
  show (SnapshotEndpoint id')          = show SnapshotsEndpoint </> id'
  show (VolumeSnapshotsEndpoint id')   = show VolumesEndpoint </> id' </> show SnapshotsEndpoint
  show VolumesActionsEndpoint          = show VolumesEndpoint </> show SnapshotsEndpoint
  show (VolumeActionsEndpoint vId)     = show VolumesEndpoint </> vId </> show ActionsEndpoint
  show (VolumeActionEndpoint vId aId)  = show VolumesEndpoint </> vId </> show ActionsEndpoint </> show aId
  show CertificatesEndpoint            = show CertificatesEndpoint
  show (CertificateEndpoint id')       = show CertificatesEndpoint </> id'
  show (DomainEndpoint name')          = show DomainsEndpoint </> name'

type VolumeId      = String
type ActionId      = Int
type CertificateId = String
type SnapshotId    = String
type DomainName    = String
