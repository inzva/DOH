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
  { _dropletLimit    :: Int
  , _floatingIpLimit :: Int
  , _email           :: String
  , _uuid            :: String
  , _emailVerified   :: Bool
  , _status          :: String
  , _statusMessage   :: String
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
