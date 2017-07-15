{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.DigitalOcean where

------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Text               as T
import           Network.HTTP.Client     hiding (Proxy)
import           Network.HTTP.Client.TLS
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Lazy    as LBS
import           System.Environment
import           Text.URI
import           Data.Maybe              (fromJust, isNothing)
import           Data.Monoid             ((<>))
import           Data.Aeson
import           Data.Proxy
------------------------------------------------
import           Network.DigitalOcean.Types

newtype Client = Client { apiKey :: BS.ByteString }

type DoErr = T.Text

newtype DO a = DO { runDO :: ReaderT Client (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader Client)

newtype DOURI = DOURI { unDOURI :: URI }

baseURI :: String
baseURI = "https://api.digitalocean.com/v2"

type Endpoint = String

data RequestMethod =
    Get
  | Post
  | Put

instance Show RequestMethod where
  show Get  = "GET"
  show Post = "POST"
  show Put  = "PUT"

makeRequest :: RequestMethod -> Endpoint -> DO LBS.ByteString
makeRequest method endp = do
  client <- ask
  let uri = baseURI <> endp
  when (isNothing $ parseURI uri) $ throwError $ "URI cannot be parsed: " <> uri
  manager <- liftIO newTlsManager
  initialRequest <- liftIO $ parseRequest uri
  let request = initialRequest { method = BSC.pack $ show method
                               , requestHeaders = [("Authorization", "Bearer " `BS.append` apiKey client)]
                               }
  liftIO $ responseBody <$> httpLbs request manager

get :: forall proxy a. (FromJSON a) => proxy a -> Endpoint -> DO a
get _ endp = do
  response <- makeRequest Get endp
  case (eitherDecode response :: Either String a) of
    Left err -> throwError err
    Right resource -> return resource

runDo' :: Client -> DO a -> IO (Either String a)
runDo' client do' = runExceptT $ runReaderT (runDO do') client

getAccounts :: DO Account
getAccounts = get (Proxy :: Proxy Account) "/account" 

getActions :: DO (PaginationState Action)
getActions = get (Proxy :: Proxy (PaginationState Action)) "/actions" 

getClient :: IO Client
getClient = Client . BSC.pack <$> getEnv "DO_TOKEN"
