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
import        Network.DigitalOcean.Utils.Actions
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

data ImageAction =
    Transfer
  | ConvertToSnapshot

instance ToJSON ImageAction where
  toJSON Transfer = object [ actionType' "transfer" ]
  toJSON ConvertToSnapshot = object [ actionType' "convert" ]

instance Payload ImageAction

data PublicImage =
    Custom String
  | Predefined PredefinedImage

instance Show PublicImage where
  show (Custom value) = show value
  show (Predefined image) = show image

instance ToJSON PublicImage where
  toJSON (Custom value) = toJSON value
  toJSON (Predefined image) = toJSON image

data PredefinedImage =
    CoreosBeta
  | Centos65x32
  | Centos65x64
  | Cassandra
  | Debian70x32
  | Debian70x64
  | Fedora24x64
  | Wordpress
  | Centos70x64
  | Debian7x64
  | Debian7x32
  | Freebsd103x64Zfs
  | Freebsd103x64
  | Ubuntu1404x64Do
  | Ubuntu1404x32Do
  | Freebsd110x64Zfs
  | Freebsd110x64
  | Rancheros
  | Centos6x32
  | Centos6x64
  | Centos7x64
  | Ubuntu1404x64
  | Ubuntu1404x32
  | Fedora25x64Atomic
  | Ubuntu1610x64
  | Ubuntu1610x32
  | Fedora25x64
  | Fedora26x64
  | CoreosStable
  | CoreosAlpha
  | Ubuntu1704x64
  | Ubuntu1704x32
  | Ubuntu1604x32
  | Ubuntu1604x64
  | Debian9x64
  | Debian8x32
  | Debian8x64
  | Fedora26x64Atomic
  | Elk
  | RubyOnRails
  | Mean
  | Redis
  | Drupal
  | Mongodb
  | Phpmyadmin
  | Mediawiki
  | Magento
  | Owncloud1604
  | Node
  | Lemp
  | Docker
  | Lamp
  | RubyOnRails1604
  | Django1604
  | Dokku1604
  | Discourse1604
  | Node1604
  | Lemp1604
  | Lamp1604
  | Mongodb1604
  | Phpmyadmin1604
  | Ghost1604
  | Mean1604
  | Mysql1604
  | Wordpress1604
  | Docker1604
  | Gitlab1604
  | Redmine

instance ToJSON PredefinedImage where
  toJSON = toJSON . show

instance Show PredefinedImage where
  show CoreosBeta        = "coreos-beta"
  show Centos65x32       = "centos-6-5-x32"
  show Centos65x64       = "centos-6-5-x64"
  show Cassandra         = "cassandra"
  show Debian70x32       = "debian-7-0-x32"
  show Debian70x64       = "debian-7-0-x64"
  show Fedora24x64       = "fedora-24-x64"
  show Wordpress         = "wordpress"
  show Centos70x64       = "centos-7-0-x64"
  show Debian7x64        = "debian-7-x64"
  show Debian7x32        = "debian-7-x32"
  show Freebsd103x64Zfs  = "freebsd-10-3-x64-zfs"
  show Freebsd103x64     = "freebsd-10-3-x64"
  show Ubuntu1404x64Do   = "ubuntu-14-04-x64-do"
  show Ubuntu1404x32Do   = "ubuntu-14-04-x32-do"
  show Freebsd110x64Zfs  = "freebsd-11-0-x64-zfs"
  show Freebsd110x64     = "freebsd-11-0-x64"
  show Rancheros         = "rancheros"
  show Centos6x32        = "centos-6-x32"
  show Centos6x64        = "centos-6-x64"
  show Centos7x64        = "centos-7-x64"
  show Ubuntu1404x64     = "ubuntu-14-04-x64"
  show Ubuntu1404x32     = "ubuntu-14-04-x32"
  show Fedora25x64Atomic = "fedora-25-x64-atomic"
  show Ubuntu1610x64     = "ubuntu-16-10-x64"
  show Ubuntu1610x32     = "ubuntu-16-10-x32"
  show Fedora25x64       = "fedora-25-x64"
  show Fedora26x64       = "fedora-26-x64"
  show CoreosStable      = "coreos-stable"
  show CoreosAlpha       = "coreos-alpha"
  show Ubuntu1704x64     = "ubuntu-17-04-x64"
  show Ubuntu1704x32     = "ubuntu-17-04-x32"
  show Ubuntu1604x32     = "ubuntu-16-04-x32"
  show Ubuntu1604x64     = "ubuntu-16-04-x64"
  show Debian9x64        = "debian-9-x64"
  show Debian8x32        = "debian-8-x32"
  show Debian8x64        = "debian-8-x64"
  show Fedora26x64Atomic = "fedora-26-x64-atomic"
  show Elk               = "elk"
  show RubyOnRails       = "ruby-on-rails"
  show Mean              = "mean"
  show Redis             = "redis"
  show Drupal            = "drupal"
  show Mongodb           = "mongodb"
  show Phpmyadmin        = "phpmyadmin"
  show Mediawiki         = "mediawiki"
  show Magento           = "magento"
  show Owncloud1604      = "owncloud-16-04"
  show Node              = "node"
  show Lemp              = "lemp"
  show Docker            = "docker"
  show Lamp              = "lamp"
  show RubyOnRails1604   = "ruby-on-rails-16-04"
  show Django1604        = "django-16-04"
  show Dokku1604         = "dokku-16-04"
  show Discourse1604     = "discourse-16-04"
  show Node1604          = "node-16-04"
  show Lemp1604          = "lemp-16-04"
  show Lamp1604          = "lamp-16-04"
  show Mongodb1604       = "mongodb-16-04"
  show Phpmyadmin1604    = "phpmyadmin-16-04"
  show Ghost1604         = "ghost-16-04"
  show Mean1604          = "mean-16-04"
  show Mysql1604         = "mysql-16-04"
  show Wordpress1604     = "wordpress-16-04"
  show Docker1604        = "docker-16-04"
  show Gitlab1604        = "gitlab-16-04"
  show Redmine           = "redmine"
