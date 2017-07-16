{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.DigitalOcean.Services.Volume where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Time.Clock
-----------------------------------------------------------------
import        Network.DigitalOcean.Services.Region
import        Network.DigitalOcean.Types
-----------------------------------------------------------------

data Volume = Volume
  { volumeId :: String
  , volumeRegion :: Region
  , volumeDropletIds :: [Int]
  , volumeName :: String
  , volumeDescription :: String
  , volumeSizeGigabytes :: Double
  , volumeCreatedAt :: UTCTime
  } deriving Show

instance FromJSON (Response [Volume]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "volumes")

instance FromJSON (Response Volume) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "volume")

instance FromJSON Volume where
  parseJSON (Object v) =
    Volume
      <$> v .: "id"
      <*> v .: "region"
      <*> v .: "droplet_ids"
      <*> v .: "name"
      <*> v .: "description"
      <*> v .: "size_gigabytes"
      <*> v .: "created_at"

data VolumePayload = VolumePayload
  { volumePayloadSizeGigabytes :: Int
  , volumePayloadName          :: String
  , volumePayloadDescripton    :: String
  , volumePayloadRegion        :: String
  , volumePayloadSnapshotId    :: String
  } deriving Show

instance ToJSON VolumePayload where
  toJSON (VolumePayload size name description region snapshotId) = 
    object [ "size_gigabytes" .= size
           , "name" .= name
           , "description" .= description
           , "region" .= region
           , "snapshot_id" .= snapshotId
           ]

instance Payload VolumePayload where

type VolumeId   = String
type DropletId  = Int
type RegionSlug = String
type VolumeName = String
type Size       = Int

data VolumeAction =
    Attach VolumeId DropletId RegionSlug
  | Detach VolumeId DropletId RegionSlug
  | Resize VolumeId Size RegionSlug
  | AttachByName VolumeName DropletId RegionSlug
  | DetachByName VolumeName DropletId RegionSlug

instance Payload VolumeAction where
instance ToJSON VolumeAction where
  toJSON (Attach _ dropletId region) =
    object [ "droplet_id".= dropletId
           , "region"    .= region
           , "type"      .= ("attach" :: String)
           ]
  toJSON (Detach _ dropletId region) =
    object [ "droplet_id" .= dropletId
           , "region"     .= region
           , "type"       .= ("detach" :: String)
           ]
  toJSON (Resize _ size region) =
    object [ "size" .= size
           , "region"     .= region
           , "type"       .= ("resize" :: String)
           ]
  toJSON (AttachByName volumeName dropletId region) =
    object [ "droplet_id" .= dropletId
           , "region"     .= region
           , "volume_name".= volumeName
           , "type"       .= ("attach" :: String)
           ]
  toJSON (DetachByName volumeName dropletId region) =
    object [ "droplet_id" .= dropletId
           , "region"     .= region
           , "volume_name".= volumeName
           , "type"       .= ("detach" :: String)
           ]
