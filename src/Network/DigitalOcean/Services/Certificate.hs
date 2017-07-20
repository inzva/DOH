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
  { certificateId               :: ActionId
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
  parseJSON (Object v) = do
    certificates <- v .: "actions"
    (next, total) <- parsePagination v
    let page = 1
    return $ PaginationState certificates page next total False

instance Paginatable Certificate where
