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
  { _slug :: String
  , _name :: String
  , _sizes :: [String] -- TODO: Make a type
  , _available :: Bool
  , _features :: [String] -- TODO: Make a type
  } deriving (Show)

instance FromJSON Region where
  parseJSON (Object v) =
    Region
      <$> v .: "slug"
      <*> v .: "name"
      <*> v .: "sizes"
      <*> v .: "available"
      <*> v .: "features"
