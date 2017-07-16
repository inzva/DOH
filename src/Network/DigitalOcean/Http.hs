{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.DigitalOcean.Http where
  
-----------------------------------------------------------------
import qualified Data.Text                 as T
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Data.Aeson
import           Data.Proxy
import           Data.Monoid
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Maybe
import           Network.HTTP.Types.Status (statusCode)
import           Text.URI
-----------------------------------------------------------------
import           Network.DigitalOcean.Types
import           Network.DigitalOcean.Utils.Pagination
-----------------------------------------------------------------

baseURI :: String
baseURI = "https://api.digitalocean.com/v2"

makeRequest :: forall proxy a p. (FromJSON a, Payload p) => proxy a -> RequestMethod -> Endpoint -> Maybe QueryParams -> Maybe p -> DO a
makeRequest _ method uri queryParams mbPayload = do
  liftIO $ print uri
  client <- ask
  let uri' = uri <> maybe mempty show queryParams
  when (isNothing $ parseURI uri') $ throwError $ "URI cannot be parsed: " <> uri'
  manager <- liftIO newTlsManager
  initialRequest <- liftIO $ parseRequest uri'
  let request = initialRequest { method = BSC.pack $ show method
                               , requestHeaders = [ ("Authorization", "Bearer " `BS.append` apiKey client)
                                                  , ("Content-Type", "application/json")
                                                  ]
                               }
  let request' = maybe request (\payload -> request { requestBody = RequestBodyLBS (encode payload) }) mbPayload
  response <- liftIO $ httpLbs request' manager
  let respStatus = statusCode $ responseStatus response
  when (respStatus < 200 || respStatus > 300) $ throwError $ "Non-success response: " <> show respStatus <> "Body:" <> show (responseBody response)
  case (eitherDecode (responseBody response) :: Either String a) of
    Left err -> throwError $ "Error occured for response body:" <> BSC.unpack (LBS.toStrict $ responseBody response) <> err
    Right resource -> return resource

get' :: forall proxy a. FromJSON a => proxy a -> String -> Maybe QueryParams -> DO a
get' _ uri queryParams = makeRequest (Proxy :: Proxy a) Get uri queryParams (Just EmptyPayload)

get :: forall proxy a. (FromJSON a) => proxy a -> Endpoint -> Maybe QueryParams -> DO a
get _ endp = get' (Proxy :: Proxy a) (baseURI <> endp)

post' :: forall proxy a p. (FromJSON a, Payload p) => proxy a -> String -> Maybe QueryParams -> p -> DO a
post' _ uri queryParams payload = makeRequest (Proxy :: Proxy a) Post uri queryParams (Just payload)

post :: forall proxy a p. (FromJSON a, Payload p) => proxy a -> Endpoint -> Maybe QueryParams -> p -> DO a
post _ endp = post' (Proxy :: Proxy a) (baseURI <> endp)

getPaginated :: forall proxy a. Paginatable a => proxy a -> Maybe PaginationConfig -> String -> DO [a]
getPaginated _ config url = 
  case config of
    Just config -> do
      let queryParams = paginationQueryParams config
      pagination <- get (Proxy :: Proxy (PaginationState a)) url (Just queryParams)
      curr <$> paginateUntil config pagination (\url -> get' (Proxy :: Proxy (PaginationState a)) url Nothing)
    Nothing ->
      curr <$> get (Proxy :: Proxy (PaginationState a)) url Nothing
