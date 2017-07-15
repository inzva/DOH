{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DigitalOcean.Types where

import GHC.Generics
import Data.Aeson

data Account = Account
  { dropletLimit    :: Int
  , floatingIpLimit :: Int
  , email           :: String
  , uuid            :: String
  , emailVerified   :: Bool
  , status          :: String
  , statusMessage   :: String
  } deriving (Show, Generic)

instance FromJSON Account where
  parseJSON (Object v) = do
    v' <- v .: "account"
    Account
      <$> v' .: "droplet_limit"
      <*> v' .: "floating_ip_limit"
      <*> v' .: "email"
      <*> v' .: "uuid"
      <*> v' .: "email_verified"
      <*> v' .: "status"
      <*> v' .: "status_message"

