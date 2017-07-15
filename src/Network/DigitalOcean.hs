{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.DigitalOcean where

------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Text                 as T
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import           System.Environment
import           Text.URI
import           Data.Maybe                (fromJust, isNothing)
import           Data.Monoid               ((<>))
import           Data.Aeson
import           Data.Proxy
import           Control.Lens
import           Data.List                 (intercalate)
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

newtype QueryParams = QueryParams [(String, String)]

instance Show QueryParams where
  show (QueryParams []) = ""
  show (QueryParams ls) = "?" <> (intercalate "&" . map (\(k, v) -> k <> "=" <> v) $ ls)

makeRequest :: forall proxy a. (FromJSON a) => proxy a -> RequestMethod -> Endpoint -> Maybe QueryParams -> DO a
makeRequest _ method uri queryParams = do
  liftIO $ print uri
  client <- ask
  let uri' = uri <> maybe mempty show queryParams
  when (isNothing $ parseURI uri') $ throwError $ "URI cannot be parsed: " <> uri'
  manager <- liftIO newTlsManager
  initialRequest <- liftIO $ parseRequest uri'
  let request = initialRequest { method = BSC.pack $ show method
                               , requestHeaders = [("Authorization", "Bearer " `BS.append` apiKey client)]
                               }
  response <- liftIO $ httpLbs request manager
  let respStatus = statusCode $ responseStatus response
  when (respStatus < 200 || respStatus > 300) $ throwError $ "Non-success response: " <> show respStatus
  case (eitherDecode (responseBody response) :: Either String a) of
    Left err -> throwError $ "Error occured for response body:" <> BSC.unpack (LBS.toStrict $ responseBody response) <> err
    Right resource -> return resource

get' :: forall proxy a. (FromJSON a) => proxy a -> String -> Maybe QueryParams -> DO a
get' _ = makeRequest (Proxy :: Proxy a) Get

get :: forall proxy a. (FromJSON a) => proxy a -> Endpoint -> Maybe QueryParams -> DO a
get _ endp = get' (Proxy :: Proxy a) (baseURI <> endp)

runDo' :: Client -> DO a -> IO (Either String a)
runDo' client do' = runExceptT $ runReaderT (runDO do') client

getAccounts :: DO Account
getAccounts = get (Proxy :: Proxy Account) "/account" Nothing

data PaginationConfig = PaginationConfig
  { pageSize :: Int
  , resultLimit :: Int
  } 

paginationQueryParams :: PaginationConfig -> QueryParams
paginationQueryParams PaginationConfig {..}  =
  QueryParams . (:[]) . ("per_page",) . show $ pageSize -- Wow, so idiomatic

getActions :: Maybe PaginationConfig -> DO Int
getActions = \case
  Just config -> do
    let queryParams = paginationQueryParams config
    pagination <- get (Proxy :: Proxy (PaginationState Action)) "/actions" (Just queryParams)
    length . curr <$> paginateUntil config pagination
  Nothing ->
    length . curr <$> get (Proxy :: Proxy (PaginationState Action)) "/actions" Nothing

paginateUntil :: (Paginatable a, FromJSON (PaginationState a)) => PaginationConfig -> PaginationState a -> DO (PaginationState a)
paginateUntil config@PaginationConfig {..} state@PaginationState {..} =
  if length curr >= resultLimit || isLast
    then
      return state
    else do
      newState <- paginate state 
      paginateUntil config newState
  
getClient :: IO Client
getClient = Client . BSC.pack <$> getEnv "DO_TOKEN"

paginate :: (Paginatable a, FromJSON (PaginationState a)) => PaginationState a -> DO (PaginationState a)
paginate s =
  case nextUrl s of
    Just url -> do
      newState <- get' (Proxy :: Proxy (PaginationState a)) url Nothing
      return $ newState { curr = curr s ++ curr newState }
    Nothing -> return s { isLast = True }
