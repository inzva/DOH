{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Image where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        Data.Time.Clock
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
import        Network.DigitalOcean.Services.Action
-----------------------------------------------------------------

data Image = Image
  { imageId            :: ImageId
  , imageName          :: String
  , imageType          :: String
  , imageDistribution  :: String
  , imageSlug          :: Maybe String
  , imagePublic        :: Bool
  , imageRegions       :: [String]
  , imageMinDiskSize   :: Int
  , imageSizeGigabytes :: Double
  , imageCreatedAt     :: UTCTime
  } deriving (Show, Generic)

instance FromJSON (Response Image) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "image")

instance FromJSON (Response [Image]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "images")

instance FromJSON Image where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (PaginationState Image) where
  parseJSON (Object v) = parsePaginationState v "images"

instance Paginatable Image

data ImageType =
  DistributionImage
  | ApplicationImage

instance Show ImageType where
  show DistributionImage = "distribution"
  show ApplicationImage = "application"

data ImageOptions = ImageOptions
  { imageType' :: Maybe ImageType -- ^ Reference: https://developers.digitalocean.com/documentation/v2/#images
  , isPrivate  :: Bool            -- ^ If True, only user's images will be returned.
  }

data ImagePayload = ImagePayload
  { imagepayloadName :: String
  } deriving (Show, Generic)

instance Payload ImagePayload

instance ToJSON ImagePayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase
