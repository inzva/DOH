{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Firewall where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        Data.Time.Clock
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
-----------------------------------------------------------------

data PendingChange = PendingChange
  { pendingchangeDropletId :: DropletId
  , pendingchangeRemoving :: Bool
  , pendingchangeStatus :: String
  } deriving (Show, Generic)

instance FromJSON PendingChange where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Protocol =
    Tcp
  | Udp
  | Icmp
  deriving Show

instance FromJSON Protocol where
  parseJSON (String s) = return $
    case s of
      "tcp"  -> Tcp
      "udp"  -> Udp
      "icmp" -> Icmp

data Channel = Channel
  { channelAddresses :: [String]
  , channelDropletIds :: [DropletId]
  , channelLoadBalancerUids :: [String]
  , channelTags :: [String]
  } deriving (Show, Generic)

instance FromJSON Channel where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data InboundRule = InboundRule
  { inboundruleProtocol :: Protocol
  , inboundrulePorts :: String
  , inboundruleSources :: Channel
  } deriving (Show, Generic)

instance FromJSON InboundRule where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data OutboundRule = OutboundRule
  { outboundruleProtocol     :: Protocol
  , outboundrulePorts        :: String
  , outboundruleDestinations :: Channel
  } deriving (Show, Generic)

instance FromJSON OutboundRule where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Firewall = Firewall
  { firewallId             :: FirewallId
  , firewallStatus         :: String
  , firewallCreatedAt      :: UTCTime
  , firewallPendingChanges :: [PendingChange]
  , firewallName           :: String
  , firewallInboundRules   :: [InboundRule]
  , firewallOutboundRules  :: [OutboundRule]
  , firewallDropletIds     :: [DropletId]
  , firewallTags           :: [String]
  } deriving (Show, Generic)

instance FromJSON (Response Firewall) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "firewall")

instance FromJSON (Response [Firewall]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "firewalls")

instance FromJSON Firewall where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (PaginationState Firewall) where
  parseJSON (Object v) = parsePaginationState v "domains"

instance Paginatable Firewall
