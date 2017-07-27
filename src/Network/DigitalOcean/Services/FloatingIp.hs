{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.FloatingIp where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        Data.Time.Clock
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
import        Network.DigitalOcean.Utils.Actions
import        Network.DigitalOcean.Services.Region
import        Network.DigitalOcean.Services.Droplet
-----------------------------------------------------------------

data FloatingIp = FloatingIp
  { floatingipIp      :: IpAddress
  , floatingipRegion  :: Region
  , floatingipDroplet :: Droplet
  } deriving (Show, Generic)

instance FromJSON (Response FloatingIp) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "floating_ip")

instance FromJSON (Response [FloatingIp]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "floating_ips")

instance FromJSON FloatingIp where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (PaginationState FloatingIp) where
  parseJSON (Object v) = parsePaginationState v "floating_ips"

instance Paginatable FloatingIp

data FloatingIpPayload =
    FloatingIpRegionPayload RegionSlug
  | FloatingIpDropletPayload DropletId
  deriving Show

instance ToJSON FloatingIpPayload where
  toJSON (FloatingIpRegionPayload regionSlug) = object [ "region" .= regionSlug ]
  toJSON (FloatingIpDropletPayload dropletId) = object [ "droplet_id" .= dropletId ]

instance Payload FloatingIpPayload

data FloatingIpAction =
    AssignToDroplet DropletId
  | Unassign
  deriving Show

instance ToJSON FloatingIpAction where
  toJSON (AssignToDroplet dropletId) = object [ actionType' "assign", "droplet_id" .= dropletId ]
  toJSON Unassign = object [ actionType' "unassign" ]

instance Payload FloatingIpAction
