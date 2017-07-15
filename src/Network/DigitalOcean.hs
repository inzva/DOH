{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.DigitalOcean where

-----------------------------------------------------------------
import           Data.Maybe                (fromJust, isNothing)
import           Data.Monoid               ((<>))
import           Control.Lens
import           Data.List                 (intercalate)
import           Data.Proxy
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
-----------------------------------------------------------------
import           Network.DigitalOcean.Types
import           Network.DigitalOcean.Http
import           Network.DigitalOcean.Utils.Pagination
import           Network.DigitalOcean.Services
-----------------------------------------------------------------

runDo' :: Client -> DO a -> IO (Either String a)
runDo' client do' = runExceptT $ runReaderT (runDO do') client

getAccounts :: DO Account
getAccounts = unResponse <$> get (Proxy :: Proxy (Response Account)) "/account" Nothing

getActions :: Maybe PaginationConfig -> DO [Action]
getActions config = getPaginated (Proxy :: Proxy Action) config "/actions"

getAction :: Int -> DO Action
getAction id' =
  unResponse <$> get (Proxy :: Proxy (Response Action)) ("/actions/" ++ show id') Nothing

getRegions :: DO [Region]
getRegions =
  get (Proxy :: Proxy [Region]) "/regions" Nothing
