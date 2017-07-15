{-# LANGUAGE DeriveGeneric #-}

module Network.DigitalOcean.Types where

import GHC.Generics

data Account = Account
  { dropletLimit    :: Int
  , floatingIpLimit :: Int
  , email           :: String
  , uuid            :: String
  , emailVerified   :: Bool
  , status          :: String
  , statusMessage   :: String
  }
