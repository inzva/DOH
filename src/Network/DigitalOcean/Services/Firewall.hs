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

-----------------------------------------------------------------

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

instance ToJSON Protocol where
  toJSON Tcp = String "tcp"
  toJSON Udp = String "udp"
  toJSON Icmp = String "icmp"

-----------------------------------------------------------------

data Channel = Channel
  { channelAddresses        :: Maybe [String]
  , channelDropletIds       :: Maybe [DropletId]
  , channelLoadBalancerUids :: Maybe [String]
  , channelTags             :: Maybe [String]
  } deriving (Show, Generic)

instance FromJSON Channel where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON Channel where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data InboundRule = InboundRule
  { inboundruleProtocol :: Protocol
  , inboundrulePorts :: String
  , inboundruleSources :: Channel
  } deriving (Show, Generic)

instance FromJSON InboundRule where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON InboundRule where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data OutboundRule = OutboundRule
  { outboundruleProtocol     :: Protocol
  , outboundrulePorts        :: String
  , outboundruleDestinations :: Channel
  } deriving (Show, Generic)

instance FromJSON OutboundRule where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON OutboundRule where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-----------------------------------------------------------------

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

-----------------------------------------------------------------

data FirewallPayload = FirewallPayload
  { name          :: String
  , inboundRules  :: [InboundRule]
  , outboundRules :: [OutboundRule]
  , dropletIds    :: Maybe [DropletId]
  , tags          :: Maybe [String]
  } deriving (Show, Generic)

instance ToJSON FirewallPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload FirewallPayload
