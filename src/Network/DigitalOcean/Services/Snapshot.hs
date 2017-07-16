{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Snapshot where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Time.Clock
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
-----------------------------------------------------------------

data Snapshot = Snapshot
  { snapshotId             :: String
  , snapshotName           :: String
  , snapshotCreatedAt      :: UTCTime
  , snapshotRegions        :: [String]
  , snapshotResourceId     :: String
  , snapshotResourceType   :: String
  , snapshotMinDiskSize    :: Double
  , snapshotSizeGigabytes  :: Double
  } deriving (Show)

instance FromJSON (Response [Snapshot]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "snapshots")

instance FromJSON (Response Snapshot) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "snapshot")

instance FromJSON Snapshot where
  parseJSON (Object v) =
    Snapshot
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "created_at"
      <*> v .: "regions"
      <*> v .: "resource_id"
      <*> v .: "resource_type"
      <*> v .: "min_disk_size"
      <*> v .: "size_gigabytes"

data SnapshotPayload = SnapshotPayload
  { name :: String }

instance ToJSON SnapshotPayload where
  toJSON (SnapshotPayload name) = 
    object [ "name" .= name ]

instance Payload SnapshotPayload where
