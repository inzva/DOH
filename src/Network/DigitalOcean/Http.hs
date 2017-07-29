{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Network.DigitalOcean.Http where
  
-----------------------------------------------------------------
import qualified Data.Text                 as T
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Client.TLS
import           System.FilePath           ((</>))
import           Data.Aeson
import           Data.Proxy
import           Data.Monoid
import           Data.Bool                 (bool)
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

type ReqWithPayload a = forall p. (Payload p) => Endpoint -> Maybe QueryParams -> p -> DO a
type ReqWithoutPayload a = Endpoint -> Maybe QueryParams -> DO a

makeRequest :: forall proxy p a. (FromJSON a, Payload p) => RequestMethod -> String -> Maybe QueryParams -> Maybe p -> DO a
makeRequest method uri queryParams mbPayload = do
  liftIO $ print uri
  liftIO $ print . encode $ mbPayload
  liftIO $ print . encode $ queryParams
  client <- ask
  let uri' = uri <> maybe mempty showQueryParams queryParams
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
  let body = bool (responseBody response) "[]" (respStatus == 204)
  case (eitherDecode body :: Either String a) of
    Left err -> throwError $ "Error occured for response body:" <> BSC.unpack (LBS.toStrict $ responseBody response) <> err
    Right resource -> return resource

{- `get` that is working with absolute uris -}
getA :: forall a. FromJSON a => String -> Maybe QueryParams -> DO a
getA uri queryParams = makeRequest Get uri queryParams (Just EmptyPayload)

get :: FromJSON a => ReqWithoutPayload a
get endp = getA (baseURI </> show endp)

get' :: FromJSON a => Endpoint -> DO a
get' endp = get endp Nothing

post :: forall a. (FromJSON a) => ReqWithPayload a
post endp q' payload = makeRequest Post (baseURI </> show endp) q' (Just payload)

delete :: ReqWithPayload ()
delete endp q' payload = makeRequest Delete (baseURI </> show endp) q' (Just payload)

delete' :: Endpoint -> DO ()
delete' endp = delete endp Nothing EmptyPayload

put :: forall a. (FromJSON a) => ReqWithPayload a
put endp q' payload = makeRequest Put (baseURI </> show endp) q' (Just payload)

getPaginated :: forall a. Paginatable a => Maybe PaginationConfig -> ReqWithoutPayload [a]
getPaginated config endp q' = do
  let queryParams = paginationQueryParams config ++
                    fromMaybe [] q'
  pagination <- get endp (Just queryParams)
  curr <$> paginateUntil (fromMaybe defaultPaginationConfig config) pagination (\url -> getA (addPaginationParam url) Nothing)
  where
    addPaginationParam = (<> maybe mempty (<> "&page_size=") (show . pageSize <$> config))
