{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Except
import Network.DigitalOcean
import Network.DigitalOcean.Types
import Network.DigitalOcean.Services
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances
import Test.QuickCheck.Property
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Aeson
import Data.Aeson.Casing
-- import Text.RawString.QQ
import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe
import Data.String
import GHC.Generics
import Data.Proxy

-----------------------------------------------------------------

prop_Response :: forall proxy service. (FromJSON service) => proxy service -> LBS.ByteString -> Property
prop_Response _ resp =
  let result = eitherDecode resp :: Either String service in
  case result of
    Left err -> error err
    Right acc -> label "wow" True

-----------------------------------------------------------------

instance Arbitrary Account where
  arbitrary =
    Account
      <$> choose (1, 100)
      <*> choose (1, 10)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> oneof (return <$> ["active", "warning", "locked"])
      <*> arbitrary

instance ToJSON Account where
  toJSON = genericToJSON $ aesonPrefix snakeCase

prop_Account :: Account -> Property
prop_Account account = prop_Response (Proxy :: Proxy Account) (encode account)

-----------------------------------------------------------------

main :: IO ()
main = return ()

-- To run tests: `quickCheck . withMaxSuccess 1000 $ prop_Account .&&. prop_Account' .&&. prop_Account`
