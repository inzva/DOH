{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.LoadBalancer where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        Data.Time.Clock
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
import        Network.DigitalOcean.Services.Region
-----------------------------------------------------------------

data LoadBalancerAlgorithm =
    RoundRobin
  | LeastConnections
  deriving (Show, Generic)

instance FromJSON LoadBalancerAlgorithm where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON LoadBalancerAlgorithm where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-----------------------------------------------------------------

data ForwardingRule = ForwardingRule
  { forwardingrulesEntryProtocol  :: String
  , forwardingrulesEntryPort      :: Int
  , forwardingrulesTargetProtocol :: String
  , forwardingrulesTargetPort     :: Int
  , forwardingrulesCertificateId  :: Maybe String
  , forwardingrulesTlsPassthrough :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON ForwardingRule where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON ForwardingRule where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload [ForwardingRule]

-----------------------------------------------------------------

data HealthCheck = HealthCheck
  { healthcheckProtocol               :: String
  , healthcheckPort                   :: Int
  , healthcheckPath                   :: Maybe String
  , healthcheckCheckIntervalSeconds   :: Maybe Int
  , healthcheckResponseTimeoutSeconds :: Maybe Int
  , healthcheckUnhealthyThreshold     :: Maybe Int
  , healthcheckHealthyThreshold       :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON HealthCheck where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON HealthCheck where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-----------------------------------------------------------------

data StickySessions = StickySessions
  { stickysessionsType             :: String
  , stickysessionsCookieName       :: String
  , stickysessionsCookieTtlSeconds :: String
  } deriving (Show, Generic)

instance FromJSON StickySessions where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON StickySessions where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-----------------------------------------------------------------

data LoadBalancer = LoadBalancer
  { loadbalancerId                  :: String
  , loadbalancerName                :: String
  , loadbalancerIp                  :: IpAddress
  , loadbalancerAlgorithm           :: LoadBalancerAlgorithm
  , loadbalancerStatus              :: String -- Make this a new type
  , loadbalancerCreatedAt           :: UTCTime
  , loadbalancerForwardingRules     :: [ForwardingRule]
  , loadbalancerHealthCheck         :: HealthCheck
  , loadbalancerStickySessions      :: StickySessions
  , loadbalancerRegion              :: Region
  , loadbalancerTag                 :: String
  , loadbalancerDropletIds          :: [DropletId]
  , loadbalancerRedirectHttpToHttps :: Bool
  } deriving (Show, Generic)

instance FromJSON LoadBalancer where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON LoadBalancer where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON (Response LoadBalancer) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "load_balancer")

instance FromJSON (Response [LoadBalancer]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "load_balancers")

instance FromJSON (PaginationState LoadBalancer) where
  parseJSON (Object v) = parsePaginationState v "load_balancers"

instance Paginatable LoadBalancer

-----------------------------------------------------------------

data LoadBalancerPayload = LoadBalancerPayload
  { loadbalancerpayloadName                :: String
  , loadbalancerpayloadAlgorithm           :: Maybe String
  , loadbalancerpayloadRegion              :: String
  , loadbalancerpayloadForwardingRules     :: [ForwardingRule]
  , loadbalancerpayloadHealthCheck         :: Maybe HealthCheck
  , loadbalancerpayloadStickySessions      :: Maybe StickySessions
  , loadbalancerpayloadRedirectHttpToHttps :: Maybe Bool
  , loadbalancerpayloadDropletIds          :: Maybe [Int]
  , loadbalancerpayloadTag                 :: String -- https://developers.digitalocean.com/documentation/v2/#create-a-new-load-balancer-with-droplet-tag
  } deriving (Show, Generic)

instance ToJSON LoadBalancerPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload LoadBalancerPayload
