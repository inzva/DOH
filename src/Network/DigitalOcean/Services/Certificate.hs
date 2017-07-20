{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Action where

-----------------------------------------------------------------
import        Data.Aeson
-- import        Data.Aeson.Types
-- import        Data.Aeson.Casing
import        Data.Time.Clock
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
import        Network.DigitalOcean.Utils.Pagination
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
    (next, total) <- parsePagination
    let page = 1
    return $ PaginationState actions page next total False

instance Paginatable Action where

type ActionId = Int

