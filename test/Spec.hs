{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Except
import Network.DigitalOcean
import Network.DigitalOcean.Types hiding (Response)
import Network.DigitalOcean.Services
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
-- import Text.RawString.QQ
import Text.InterpolatedString.Perl6 (q)

client = Client "wow"

data Response = Response LBS.ByteString

accStr :: LBS.ByteString
accStr = [q|
{
	"droplet_limit": 25,
	"floating_ip_limit": 5,
	"email": "sammy@digitalocean.com",
	"uuid": "b6fr89dbf6d9156cace5f3c78dc9851d957381ef",
	"email_verified": true,
	"status": "active",
	"status_message": ""
}|]

wowResp :: Response
wowResp = Response accStr

getAccounts' :: (Response -> IO Account) -> IO Account
getAccounts' f = f wowResp

-- mockedAccounts :: Identity [Account]
prop_Wow = monadicIO $ do
  account <- run $ getAccounts' $ \(Response resp) ->
    case eitherDecode resp of
      Left err -> error err
      Right acc -> return acc
  assert True

main = return ()  
