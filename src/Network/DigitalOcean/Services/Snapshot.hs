{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.DigitalOcean.Services.Snapshot where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        Data.Time.Clock
import        GHC.Generics
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
  } deriving (Show, Generic)

instance FromJSON (Response [Snapshot]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "snapshots")

instance FromJSON (Response Snapshot) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "snapshot")

instance FromJSON Snapshot where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype SnapshotPayload = SnapshotPayload
  { snapshotpayloadName :: String
  } deriving (Show, Generic)

instance ToJSON SnapshotPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload SnapshotPayload where
