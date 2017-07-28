{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.DigitalOcean.Services.Volume where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        Data.Time.Clock
import        GHC.Generics
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
  } deriving (Show, Generic)

instance FromJSON (Response [Volume]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "volumes")

instance FromJSON (Response Volume) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "volume")

instance FromJSON Volume where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data VolumePayload = VolumePayload
  { volumepayloadSizeGigabytes :: Int
  , volumepayloadName          :: String
  , volumepayloadDescripton    :: String
  , volumepayloadRegion        :: String
  , volumepayloadSnapshotId    :: String
  } deriving (Show, Generic)

instance ToJSON VolumePayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload VolumePayload

type VolumeName = String

data VolumeAction =
    Attach VolumeId DropletId (Maybe RegionSlug)
  | Detach VolumeId DropletId (Maybe RegionSlug)
  | ResizeVolume VolumeId Int (Maybe RegionSlug)
  | AttachByName VolumeName DropletId (Maybe RegionSlug)
  | DetachByName VolumeName DropletId (Maybe RegionSlug)

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
  toJSON (ResizeVolume _ size region) =
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
