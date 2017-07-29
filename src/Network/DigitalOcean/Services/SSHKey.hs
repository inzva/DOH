{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.DigitalOcean.Services.SSHKey where

-----------------------------------------------------------------
import        Data.Aeson
import        Data.Aeson.Casing
import        GHC.Generics
-----------------------------------------------------------------
import        Network.DigitalOcean.Types
-----------------------------------------------------------------

data SSHKey = SSHKey
  { sshkeyId          :: SSHKeyId
  , sshkeyFingerprint :: String
  , sshkeyPublicKey   :: String
  , sshkeyName        :: String
  } deriving (Show, Generic)

instance FromJSON SSHKey where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON SSHKey where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON (Response SSHKey) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "ssh_key")

instance FromJSON (Response [SSHKey]) where
  parseJSON (Object v) =
    fmap Response $ parseJSON =<< (v .: "ssh_keys")

-----------------------------------------------------------------

data SSHKeyPayload = SSHKeyPayload
  { sshkeypayloadName      :: String
  , sshkeypayloadPublicKey :: String
  } deriving (Show, Generic)

instance ToJSON SSHKeyPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload SSHKeyPayload

-----------------------------------------------------------------

data SSHKeyNamePayload = SSHKeyNamePayload
  { sshkeynamepayloadName :: String
  } deriving (Show, Generic)

instance ToJSON SSHKeyNamePayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Payload SSHKeyNamePayload
