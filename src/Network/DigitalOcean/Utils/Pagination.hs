{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.DigitalOcean.Utils.Pagination where

-----------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                  as T
-----------------------------------------------------------------
import           Network.DigitalOcean.Types
import qualified Data.Set                   as Set
-----------------------------------------------------------------

paginate :: (Paginatable a, FromJSON (PaginationState a)) => (String -> DO (PaginationState a)) -> PaginationState a -> DO (PaginationState a)
paginate f s =
  case nextUrl s of
    Just url -> do
      newState <- f url
      return $ newState { curr = curr s ++ curr newState }
    Nothing -> return s { isLast = True }
    
paginationQueryParams :: PaginationConfig -> QueryParams
paginationQueryParams (PaginationConfig pageSize _)  = Set.singleton . ("per_page",) . show $ pageSize

paginateUntil :: (Paginatable a, FromJSON (PaginationState a)) => PaginationConfig -> PaginationState a -> (String -> DO (PaginationState a)) -> DO (PaginationState a)
paginateUntil config@PaginationConfig {..} state@PaginationState {..} f =
  if length curr >= resultLimit || isLast
    then
      return state
    else do
      newState <- paginate f state 
      paginateUntil config newState f

parsePaginationState :: Paginatable a => Object -> T.Text -> Parser (PaginationState a)
parsePaginationState v key = do
  values <- v .: key
  (next, total) <- parse_meta
  let page = 1
  return $ PaginationState values page next total False
  where
    parse_meta :: Parser (Maybe String, Int)
    parse_meta = do
      links <- v .: "links"
      pages <- links .: "pages"
      (,)
        <$> (pages .:? "next")
        <*> (v .: "meta" >>= (.: "total"))
