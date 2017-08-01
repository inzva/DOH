{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Droplet where

-----------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics
import           Data.Time.Clock
import           Data.Map
import qualified Data.HashMap.Lazy as HML
-----------------------------------------------------------------
import           Network.DigitalOcean.Types
import           Network.DigitalOcean.Utils.Pagination
import           Network.DigitalOcean.Utils.Actions
import           Network.DigitalOcean.Services.Region
import           Network.DigitalOcean.Services.Image
import           Network.DigitalOcean.Services.Size
-----------------------------------------------------------------

data Kernel = Kernel
  { kernelId :: Int
  , kernelName :: String
  , kernelVersion :: String
  } deriving (Show, Generic)

instance FromJSON Kernel where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (Response [Kernel]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "kernels")

-----------------------------------------------------------------

data DropletStatus =
    New
  | Active
  | Off
  | Archive
  deriving (Show, Generic)

instance FromJSON DropletStatus where
  parseJSON "new"     = return New
  parseJSON "active"  = return Active
  parseJSON "off"     = return Off
  parseJSON "archive" = return Archive

-----------------------------------------------------------------

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

-----------------------------------------------------------------

newtype Backup = Backup Image
  deriving Show

instance FromJSON Backup where
  parseJSON v = Backup <$> parseJSON v

instance FromJSON (Response [Backup]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "backups")
  
-----------------------------------------------------------------

newtype Neighbors = Neighbors [[Droplet]]
  deriving Show

instance FromJSON Neighbors where
  parseJSON v = Neighbors <$> parseJSON v

instance FromJSON (Response Neighbors) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "neighbors")

-----------------------------------------------------------------

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

data DropletPayload =
    SingleDropletPayload String IDropletPayload
  | MultipleDropletPayload [String] IDropletPayload

{- https://developers.digitalocean.com/documentation/v2/#create-a-new-droplet -}
data IDropletPayload = IDropletPayload
  { dropletpayloadRegion            :: String
  , dropletpayloadSize              :: String
  , dropletpayloadImage             :: PublicImage -- ^ number (if using an image ID), or String (if using a public image slug)
  , dropletpayloadSshKeys           :: Maybe [String]
  , dropletpayloadBackups           :: Maybe Bool
  , dropletpayloadIpv6              :: Maybe Bool
  , dropletpayloadPrivateNetworking :: Maybe Bool
  , dropletpayloadUserData          :: Maybe String
  , dropletpayloadMonitoring        :: Maybe Bool
  , dropletpayloadVolumes           :: Maybe [String]
  , dropletpayloadTags              :: Maybe [String]
  } deriving (Show, Generic)

instance ToJSON IDropletPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

mergeAeson :: Value -> Value -> Value
mergeAeson (Object x) (Object y) = Object $ HML.unions [x, y]

instance ToJSON DropletPayload where
  toJSON (SingleDropletPayload name payload) =
    object [ "name" .= name ] `mergeAeson` toJSON payload
  toJSON (MultipleDropletPayload names payload) =
    object [ "names" .= names ] `mergeAeson` toJSON payload

instance Payload DropletPayload

-----------------------------------------------------------------

data DropletAction =
    EnableBackups
  | DisableBackups
  | Reboot
  | PowerCycle
  | Shutdown
  | PowerOff
  | PowerOn
  | Restore String
  | PasswordReset
  | ResizeDroplet (Maybe Bool) String
  | Rebuild String
  | Rename String
  | ChangeKernel Int
  | EnableIpV6
  | EnablePrivateNetworking
  | TakeSnapshot (Maybe String)
  deriving (Eq, Show)

{- Reference:
 - https://developers.digitalocean.com/documentation/v2/#acting-on-tagged-droplets
 -}
actionAllowedAsBulk :: DropletAction -> Bool
actionAllowedAsBulk PowerCycle              = True
actionAllowedAsBulk PowerOn                 = True
actionAllowedAsBulk PowerOff                = True
actionAllowedAsBulk Shutdown                = True
actionAllowedAsBulk EnablePrivateNetworking = True
actionAllowedAsBulk EnableIpV6              = True
actionAllowedAsBulk EnableBackups           = True
actionAllowedAsBulk DisableBackups          = True
actionAllowedAsBulk TakeSnapshot {}         = True
actionAllowedAsBulk _                       = False

instance ToJSON DropletAction where
  toJSON EnableBackups             = object [ actionType' "enable_backups" ]
  toJSON DisableBackups            = object [ actionType' "disable_backups" ]
  toJSON Reboot                    = object [ actionType' "reboot" ]
  toJSON PowerCycle                = object [ actionType' "power_cycle" ]
  toJSON Shutdown                  = object [ actionType' "shutdown" ]
  toJSON PowerOff                  = object [ actionType' "power_off" ]
  toJSON PowerOn                   = object [ actionType' "power_on" ]
  toJSON (Restore imgId)           = object [ actionType' "restore", "image" .= imgId ]
  toJSON PasswordReset             = object [ actionType' "password_reset" ]
  toJSON (ResizeDroplet disk size) = object [ actionType' "resize", "disk" .= disk, "size" .= size ]
  toJSON (Rebuild imgId)           = object [ actionType' "rebuild", "image" .= imgId ]
  toJSON (Rename name)             = object [ actionType' "rename", "name" .= name ]
  toJSON (ChangeKernel id')        = object [ actionType' "change_kernel", "kernel" .= id' ]
  toJSON EnableIpV6                = object [ actionType' "enable_ipv6" ]
  toJSON EnablePrivateNetworking   = object [ actionType' "enable_private_networking" ]
  toJSON (TakeSnapshot name)       = object [ actionType' "snapshot", "name" .= name ]

instance Payload DropletAction

