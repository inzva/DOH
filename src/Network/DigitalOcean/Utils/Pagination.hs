{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module Network.DigitalOcean.Utils.Pagination where

-----------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                  as T
import qualified Data.Text                  as T
-----------------------------------------------------------------
import           Network.DigitalOcean.Types
import qualified Data.Set                   as Set
-----------------------------------------------------------------

paginate
  :: (Paginatable a, FromJSON (PaginationState a))
  => (String -> DO (PaginationState a))
  -> PaginationState a
  -> DO (PaginationState a)
paginate f s =
  case nextUrl s of
    Just url -> do
      newState <- f url
      return $ newState { curr = curr s ++ curr newState }
    Nothing -> return s { isLast = True }
    
paginationQueryParams :: Maybe PaginationConfig -> QueryParams
paginationQueryParams Nothing = []
paginationQueryParams (Just (PaginationConfig pageSize _)) = [("per_page", show pageSize)]

paginateUntil
  :: (Paginatable a, FromJSON (PaginationState a))
  => PaginationConfig
  -> PaginationState a
  -> (String -> DO (PaginationState a))
  -> DO (PaginationState a)
paginateUntil config@PaginationConfig {..} state@PaginationState {..} f =
  if length curr >= resultLimit || isLast
    then
      return state
    else do
      newState <- paginate f state 
      paginateUntil config newState f

-- Taken from: https://stackoverflow.com/questions/28368980/composing-optional-aeson-parsers
(/?) :: FromJSON a => Parser (Maybe Object) -> T.Text -> Parser (Maybe a)
maybeParser /? key = maybeParser >>= maybe (return Nothing) (.:? key)

parsePaginationState :: Paginatable a => Object -> T.Text -> Parser (PaginationState a)
parsePaginationState v key = do
  values <- v .: key
  (next, total) <- parse_meta
  let page = 1
  return $ PaginationState values page next total False
  where
    parse_meta :: Parser (Maybe String, Maybe Int)
    parse_meta =
      (,)
        <$> v .:? "links" /? "pages" /? "next"
        <*> (v .:? "meta" /? "total")
