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

instance Arbitrary Action where
  arbitrary =
    Action
      <$> choose (1, 100000000)
      <*> oneof (return <$> ["in-progress", "completed", "errored"])
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> choose (1, 100000000)
      <*> arbitrary
      <*> arbitrary

instance ToJSON Action where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Arbitrary (Response Action) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response Action) where
  toJSON (Response action) = object [ "action" .= action]

instance Arbitrary (Response [Action]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Action]) where
  toJSON (Response actions) = object [ "actions" .= actions]

prop_Action :: Action -> Response Action -> Response [Action] -> Property
prop_Action v v' v'' = prop_Response (Proxy :: Proxy Action) (encode v)
                           .&&. prop_Response (Proxy :: Proxy (Response Action)) (encode v')
                           .&&. prop_Response (Proxy :: Proxy (Response [Action])) (encode v'')

-----------------------------------------------------------------

instance Arbitrary Certificate where
  arbitrary =
    Certificate
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ToJSON Certificate where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Arbitrary (Response Certificate) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response Certificate) where
  toJSON (Response certificate) = object [ "certificate" .= certificate]

instance Arbitrary (Response [Certificate]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Certificate]) where
  toJSON (Response certificates) = object [ "certificates" .= certificates]

prop_Certificate :: Certificate -> Response Certificate -> Response [Certificate] -> Property
prop_Certificate v v' v'' = prop_Response (Proxy :: Proxy Certificate) (encode v)
                       .&&. prop_Response (Proxy :: Proxy (Response Certificate)) (encode v')
                       .&&. prop_Response (Proxy :: Proxy (Response [Certificate])) (encode v'')

-----------------------------------------------------------------

instance Arbitrary Region where
  arbitrary =
    Region
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

prop_Region :: Region -> Property
prop_Region = prop_Response (Proxy :: Proxy Region) . encode

instance Arbitrary (Response [Region]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Region]) where
  toJSON (Response regions) = object [ "regions" .= regions]

prop_RegionsResponse :: Response [Region] -> Property
prop_RegionsResponse = prop_Response (Proxy :: Proxy (Response [Region])) . encode

-----------------------------------------------------------------

instance Arbitrary Snapshot where
  arbitrary =
    Snapshot
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ToJSON Snapshot where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Arbitrary (Response Snapshot) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response Snapshot) where
  toJSON (Response snapshot) = object [ "snapshot" .= snapshot]

instance Arbitrary (Response [Snapshot]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Snapshot]) where
  toJSON (Response snapshots) = object [ "snapshots" .= snapshots]

prop_Snapshot :: Snapshot -> Response Snapshot -> Response [Snapshot] -> Property
prop_Snapshot v v' v'' = prop_Response (Proxy :: Proxy Snapshot) (encode v)
                  .&&. prop_Response (Proxy :: Proxy (Response Snapshot)) (encode v')
                  .&&. prop_Response (Proxy :: Proxy (Response [Snapshot])) (encode v'')

-----------------------------------------------------------------

instance Arbitrary Volume where
  arbitrary =
    Volume
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ToJSON Volume where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Arbitrary (Response Volume) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response Volume) where
  toJSON (Response volume) = object [ "volume" .= volume]

instance Arbitrary (Response [Volume]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Volume]) where
  toJSON (Response volumes) = object [ "volumes" .= volumes]

prop_Volume :: Volume -> Response Volume -> Response [Volume] -> Property
prop_Volume v v' v'' = prop_Response (Proxy :: Proxy Volume) (encode v)
                  .&&. prop_Response (Proxy :: Proxy (Response Volume)) (encode v')
                  .&&. prop_Response (Proxy :: Proxy (Response [Volume])) (encode v'')

-----------------------------------------------------------------

runTests = quickCheck . withMaxSuccess 100 $ prop_Account
                                         .&&. prop_Action
                                         .&&. prop_Certificate
                                         .&&. prop_Region
                                         .&&. prop_RegionsResponse
                                         .&&. prop_Snapshot
                                         .&&. prop_Volume

main :: IO ()
main = return ()
