{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.DigitalOcean.Services.Action where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Time.Clock
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
-----------------------------------------------------------------

data Certificate = Certificate
  { certificateId               :: ActionId
  , certificateName             :: String
  , certificateNotAfter         :: String
  , certificateSha1Fingerprint  :: String
  , certificateCreatedAt        :: UTCTime
  } deriving (Show)

instance FromJSON (Response Certificate) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "certificate")

instance FromJSON (Response [Certificate]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "certificates")

instance FromJSON Certificate where
  parseJSON (Object v) =
    Certificate
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "not_after"
      <*> v .: "sha1_fingerprint"
      <*> v .: "created_at"

parsePagination :: Object -> Parser (Int, Maybe String, Int)
parsePagination v = do
  links <- v .: "links"
  pages <- links .: "pages"
  (pages,,)
    <$> (pages .:? "next")
    <*> (v .: "meta" >>= (.: "total"))

instance FromJSON (PaginationState Certificate) where
  parseJSON (Object v) = do
    certificates <- v .: "actions"
    (pages, next, total) <- parsePagination
    let page = 1
    return $ PaginationState actions page next total False

instance Paginatable Action where

type ActionId = Int

