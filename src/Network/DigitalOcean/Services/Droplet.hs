{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Droplet where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        GHC.Generics
import        Data.Time.Clock
import        Data.Map
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
import        Network.DigitalOcean.Services.Region
import        Network.DigitalOcean.Services.Image
import        Network.DigitalOcean.Services.Size
-----------------------------------------------------------------

data Kernel = Kernel
  { kernelId :: Int
  , kernelName :: String
  , kernelVersion :: String
  } deriving (Show, Generic)

instance FromJSON Kernel where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data DropletStatus =
    New
  | Active
  | Off
  | Archive
  deriving (Show, Generic)

instance FromJSON DropletStatus where
  parseJSON "new" = return New
  parseJSON "active" = return Active
  parseJSON "off" = return Off
  parseJSON "archive" = return Archive

type IpAddress = String

data Network = Network
  { networkIpAddress :: IpAddress
  , networkNetmask   :: IpAddress
  , networkGateway   :: IpAddress
  , networkType      :: String -- ^ Whether there is another type of network is available is not documented in DO API.
  } deriving (Show, Generic)

instance FromJSON Network where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Networks = Networks
  { v4 :: [Network]
  , v6 :: [Network]
  } deriving (Show)

instance FromJSON Networks where
  parseJSON (Object v) = Networks
    <$> v .: "v4"
    <*> v .: "v6"

data Droplet = Droplet
  { dropletId          :: DropletId
  , dropletName        :: String
  , dropletMemory      :: Int
  , dropletVcpus       :: Int
  , dropletDisk        :: Int
  , dropletLocked      :: Bool
  , dropletCreatedAt   :: UTCTime
  , dropletStatus      :: DropletStatus
  , dropletBackupIds   :: [Int]
  , dropletSnapshotIds :: [SnapshotId]
  , dropletFeatures    :: [String]
  , dropletRegion      :: Region
  , dropletImage       :: Image
  , dropletSize        :: Size
  , dropletSizeSlug    :: String
  , dropletNetworks    :: Networks
  , dropletKernel      :: Maybe Kernel
  --droplet , nextBackupWindow :: Maybe BackupWindow -- No example for this yet
  , dropletTags        :: [String]
  , dropletVolumeIds   :: Maybe [VolumeId]
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
