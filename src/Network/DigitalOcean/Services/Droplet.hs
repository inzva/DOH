{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Droplet where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        GHC.Generics
import        Data.Time.Clock
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
import        Network.DigitalOcean.Services.Region
import        Network.DigitalOcean.Services.Image
-----------------------------------------------------------------

data DropletStatus =
    New
  | Active
  | Off
  | Archive
  deriving (Show, Generic)

instance FromJSON where
  parseJSON "new" = return New
  parseJSON "active" = return Active
  parseJSON "off" = return Off
  parseJSON "archive" = return Archive

data Droplet = Droplet
  { id :: DropletId
  , name :: String
  , memory :: Int
  , vcpus :: Int
  , disk :: Int
  , locked :: Bool
  , createdAt :: UTCTime
  , status :: DropletStatus
  , backupIds :: [Int]
  , snapshotIds :: [SnapshotId]
  , features :: [String]
  , region :: Region
  , image :: Image
  , size :: Size
  , sizeSlug :: String
  , networks :: Map NetworkKind Network
  , kernel :: Maybe Kernel
  -- , nextBackupWindow :: Maybe BackupWindow -- No example for this yet
  , tags :: [String]
  , volumeIds :: [VolumeId]
  } deriving (Show, Generic)

instance FromJSON (Response Droplet) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "droplet")

instance FromJSON (Response [Droplet]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "droplets")

instance FromJSON Droplet where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (PaginationState Droplet) where
  parseJSON (Object v) = parsePaginationState v "droplets"

instance Paginatable Droplet

-- instance ToJSON DropletPayload where
--   toJSON = genericToJSON $ aesonPrefix snakeCase

-- instance Payload DropletPayload
