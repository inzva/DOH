{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Domain where

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

data Domain = Domain
  { domainName     :: String
  , domainTtl      :: String
  , domainZoneFile :: String
  } deriving (Show, Generic)

instance FromJSON (Response Domain) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "domain")

instance FromJSON (Response [Domain]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "domains")

instance FromJSON Domain where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (PaginationState Domain) where
  parseJSON (Object v) = parsePaginationState v "domains"

instance Paginatable Domain

data Domainpayload = Domainpayload
  { domainpayloadName      :: String
  , domainpayloadIpAddress :: String
  } deriving (Show, Generic)

instance ToJSON Domainpayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload Domainpayload
