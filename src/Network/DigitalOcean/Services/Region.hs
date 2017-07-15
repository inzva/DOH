{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Region where

-----------------------------------------------------------------
import        Data.Aeson
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
-----------------------------------------------------------------

instance FromJSON (Response [Region]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "regions")

data Region = Region
  { regionSlug      :: String
  , regionName      :: String
  , regionSizes     :: [String] -- TODO: Make a type
  , regionAvailable :: Bool
  , regionFeatures  :: [String] -- TODO: Make a type
  } deriving (Show)

instance FromJSON Region where
  parseJSON (Object v) =
    Region
      <$> v .: "slug"
      <*> v .: "name"
      <*> v .: "sizes"
      <*> v .: "available"
      <*> v .: "features"
