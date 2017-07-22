{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Certificate where

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

data Certificate = Certificate
  { certificateId               :: CertificateId
  , certificateName             :: String
  , certificateNotAfter         :: String
  , certificateSha1Fingerprint  :: String
  , certificateCreatedAt        :: UTCTime
  } deriving (Show, Generic)

instance FromJSON (Response Certificate) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "certificate")

instance FromJSON (Response [Certificate]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "certificates")

instance FromJSON Certificate where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (PaginationState Certificate) where
  parseJSON (Object v) = parsePaginationState v "certificates"

instance Paginatable Certificate where

data Certificatepayload = Certificatepayload
  { certificatepayloadName :: String
  , certificatepayloadPrivateKey :: String
  , certificatepayloadLeafCertificate :: String
  , certificatepayloadCertificateChain :: Maybe String
  } deriving (Show, Generic)

instance ToJSON Certificatepayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload Certificatepayload
