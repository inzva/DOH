{-# LANGUAGE OverloadedStrings #-}

module Network.DigitalOcean.Services.Region where

import Data.Aeson

data Region = Region
  { _slug :: String
  , _name :: String
  , _sizes :: [String] -- TODO: Make a type
  , _available :: Bool
  , _features :: [String] -- TODO: Make a type
  }

instance FromJSON Region where
  parseJSON (Object v) =
    Region
      <$> v .: "slug"
      <*> v .: "name"
      <*> v .: "sizesj"
      <*> v .: "available"
      <*> v .: "features"
