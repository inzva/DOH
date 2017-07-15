{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DigitalOcean.Services.Account where

-----------------------------------------------------------------
import        Data.Aeson
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
  parseJSON (Object v) =
    Account
      <$> v .: "droplet_limit"
      <*> v .: "floating_ip_limit"
      <*> v .: "email"
      <*> v .: "uuid"
      <*> v .: "email_verified"
      <*> v .: "status"
      <*> v .: "status_message"
