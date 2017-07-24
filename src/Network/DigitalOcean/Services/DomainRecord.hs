{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.DomainRecord where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
import        Network.DigitalOcean.Services.Action
-----------------------------------------------------------------

data DomainRecord = DomainRecord
  { domainrecordId       :: Int
  , domainrecordType     :: String
  , domainrecordName     :: String
  , domainrecordData     :: String
  , domainrecordPriority :: Maybe Int
  , domainrecordPort     :: Maybe Int
  , domainrecordTtl      :: Int
  , domainrecordWeight   :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON (Response DomainRecord) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "domain_record")

instance FromJSON (Response [DomainRecord]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "domain_records")

instance FromJSON DomainRecord where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (PaginationState DomainRecord) where
  parseJSON (Object v) = parsePaginationState v "domain_records"

instance Paginatable DomainRecord

data DomainRecordPayload = DomainRecordPayload
  { domainrecordpayloadType     :: String
  , domainrecordpayloadName     :: String
  , domainrecordpayloadData     :: String
  , domainrecordpayloadPriority :: Maybe Int
  , domainrecordpayloadPort     :: Maybe Int
  , domainrecordpayloadTtl      :: Int -- Should this be maybe? https://developers.digitalocean.com/documentation/v2/#create-a-new-domain-record
  , domainrecordpayloadWeight   :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON DomainRecordPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload DomainRecordPayload

