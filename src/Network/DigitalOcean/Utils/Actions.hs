{-# LANGUAGE OverloadedStrings #-}

module Network.DigitalOcean.Utils.Actions (actionType') where

import Data.Aeson

actionType' :: KeyValue kv => String -> kv
actionType' = (.=) "type"
