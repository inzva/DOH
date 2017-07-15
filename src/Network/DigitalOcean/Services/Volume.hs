{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

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
  , volumeSizeGigabytes :: Int
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
