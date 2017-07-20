{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DigitalOcean.Services.Account where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
-----------------------------------------------------------------

data Account = Account
  { accountDropletLimit    :: Int
  , accountFloatingIpLimit :: Int
  , accountEmail           :: String
  , accountUuid            :: String
  , accountEmailVerified   :: Bool
  , accountStatus          :: String
  , accountStatusMessage   :: String
  } deriving (Show, Generic)

instance FromJSON (Response Account) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "account")

instance FromJSON Account where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
