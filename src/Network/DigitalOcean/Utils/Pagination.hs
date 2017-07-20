{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.DigitalOcean.Utils.Pagination where

-----------------------------------------------------------------
import         Data.Aeson
import         Data.Aeson.Types
-----------------------------------------------------------------
import         Network.DigitalOcean.Types
-----------------------------------------------------------------

paginate :: (Paginatable a, FromJSON (PaginationState a)) => (String -> DO (PaginationState a)) -> PaginationState a -> DO (PaginationState a)
paginate f s =
  case nextUrl s of
    Just url -> do
      newState <- f url
      return $ newState { curr = curr s ++ curr newState }
    Nothing -> return s { isLast = True }
    
paginationQueryParams :: PaginationConfig -> QueryParams
paginationQueryParams PaginationConfig {..}  =
  QueryParams . (:[]) . ("per_page",) . show $ pageSize -- Wow, so idiomatic

paginateUntil :: (Paginatable a, FromJSON (PaginationState a)) => PaginationConfig -> PaginationState a -> (String -> DO (PaginationState a)) -> DO (PaginationState a)
paginateUntil config@PaginationConfig {..} state@PaginationState {..} f =
  if length curr >= resultLimit || isLast
    then
      return state
    else do
      newState <- paginate f state 
      paginateUntil config newState f

parsePagination :: Object -> Parser (Maybe String, Int)
parsePagination v = do
  links <- v .: "links"
  pages <- links .: "pages"
  (,)
    <$> (pages .:? "next")
    <*> (v .: "meta" >>= (.: "total"))


