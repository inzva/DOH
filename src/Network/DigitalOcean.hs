{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DigitalOcean where

------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Lens.Setter
import           Control.Lens.Getter
import qualified Data.Text               as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
import           System.Environment
import           Text.URI
import           Data.Maybe              (fromJust, isNothing)
import           Data.Monoid             ((<>))
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

get :: Endpoint -> DO ()
get endp = do
  client <- ask
  let uri = baseURI <> endp
  when (isNothing $ parseURI uri) $ throwError $ "URI cannot be parsed: " <> uri
  liftIO $ do 
    print uri
    manager <- newTlsManager
    initialRequest <- parseRequest uri
    let request = initialRequest { method = "GET", requestHeaders = [("Authorization", "Bearer " `BS.append` apiKey client)] }
    response <- httpLbs request manager
    print response

runReq :: Client -> DO a -> IO (Either String a)
runReq client do' = runExceptT $ runReaderT (runDO do') client

getAccounts :: DO ()
getAccounts = get "/account"

getClient :: IO Client
getClient = Client . BSC.pack <$> getEnv "DO_TOKEN"
