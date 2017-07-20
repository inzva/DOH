{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.DigitalOcean.Services.Region where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
-----------------------------------------------------------------

data Region = Region
  { regionSlug      :: String
  , regionName      :: String
  , regionSizes     :: [String] -- TODO: Make a type
  , regionAvailable :: Bool
  , regionFeatures  :: [String] -- TODO: Make a type
  } deriving (Show, Generic)

instance FromJSON (Response [Region]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "regions")

instance FromJSON Region where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
