{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DigitalOcean where

------------------------------------------------

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Text               as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
import           System.Environment

------------------------------------------------

data Client = Client { apiKey :: BS.ByteString }

type DoErr = T.Text

newtype DO a = DO { runDO :: ReaderT Client (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader Client)

get :: IO ()
get = do
  manager <- newTlsManager
  initialRequest <- parseRequest "https://api.digitalocean.com/v2/account" 
  client <- getClient
  let request = initialRequest { method = "GET", requestHeaders = [("Authorization", "Bearer " `BS.append` apiKey client)] }
  response <- httpLbs request manager
  print response

getClient :: IO Client
getClient = Client . BSC.pack <$> getEnv "DO_TOKEN"
