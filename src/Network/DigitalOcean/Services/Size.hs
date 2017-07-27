{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.DigitalOcean.Services.Size where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
-----------------------------------------------------------------

data Size = Size
  { sizeSlug         :: String
  , sizeAvailable    :: Bool
  , sizeTransfer     :: Double
  , sizePriceMonthly :: Double
  , sizePriceHourly  :: Double
  , sizeMemory       :: Int
  , sizeVcpus        :: Int
  , sizeDisk         :: Int
  -- , sizeRegions      :: [String]
  } deriving (Show, Generic)

instance FromJSON (Response [Size]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "sizes")

instance FromJSON Size where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

