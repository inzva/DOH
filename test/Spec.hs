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

prop_Action :: Action -> Property
prop_Action = prop_Response (Proxy :: Proxy Action) . encode

instance Arbitrary (Response Action) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response Action) where
  toJSON (Response action) = object [ "action" .= action]

prop_ActionResponse :: Response Action -> Property
prop_ActionResponse = prop_Response (Proxy :: Proxy (Response Action)) . encode

instance Arbitrary (Response [Action]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Action]) where
  toJSON (Response actions) = object [ "actions" .= actions]

prop_ActionsResponse :: Response [Action] -> Property
prop_ActionsResponse = prop_Response (Proxy :: Proxy (Response [Action])) . encode

-----------------------------------------------------------------

instance Arbitrary Certificate where
  arbitrary =
    Certificate
      <$> choose (1, 100000000)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ToJSON Certificate where
  toJSON = genericToJSON $ aesonPrefix snakeCase

prop_Certificate :: Certificate -> Property
prop_Certificate = prop_Response (Proxy :: Proxy Certificate) . encode

instance Arbitrary (Response Certificate) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response Certificate) where
  toJSON (Response certificate) = object [ "certificate" .= certificate]

prop_CertificateResponse :: Response Certificate -> Property
prop_CertificateResponse = prop_Response (Proxy :: Proxy (Response Certificate)) . encode

instance Arbitrary (Response [Certificate]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Certificate]) where
  toJSON (Response certificates) = object [ "certificates" .= certificates]

prop_CertificatesResponse :: Response [Certificate] -> Property
prop_CertificatesResponse = prop_Response (Proxy :: Proxy (Response [Certificate])) . encode

-----------------------------------------------------------------

instance Arbitrary Region where
  arbitrary =
    Region
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ToJSON Region where
  toJSON = genericToJSON $ aesonPrefix snakeCase

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

prop_Snapshot :: Snapshot -> Property
prop_Snapshot = prop_Response (Proxy :: Proxy Snapshot) . encode

instance Arbitrary (Response Snapshot) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response Snapshot) where
  toJSON (Response snapshot) = object [ "snapshot" .= snapshot]

prop_SnapshotResponse :: Response Snapshot -> Property
prop_SnapshotResponse = prop_Response (Proxy :: Proxy (Response Snapshot)) . encode

instance Arbitrary (Response [Snapshot]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Snapshot]) where
  toJSON (Response snapshots) = object [ "snapshots" .= snapshots]

prop_SnapshotsResponse :: Response [Snapshot] -> Property
prop_SnapshotsResponse = prop_Response (Proxy :: Proxy (Response [Snapshot])) . encode

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

prop_Volume :: Volume -> Property
prop_Volume = prop_Response (Proxy :: Proxy Volume) . encode

instance Arbitrary (Response Volume) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response Volume) where
  toJSON (Response volume) = object [ "volume" .= volume]

prop_VolumeResponse :: Response Volume -> Property
prop_VolumeResponse = prop_Response (Proxy :: Proxy (Response Volume)) . encode

instance Arbitrary (Response [Volume]) where
  arbitrary = Response <$> arbitrary

instance ToJSON (Response [Volume]) where
  toJSON (Response volumes) = object [ "volumes" .= volumes]

prop_VolumesResponse :: Response [Volume] -> Property
prop_VolumesResponse = prop_Response (Proxy :: Proxy (Response [Volume])) . encode

-----------------------------------------------------------------

instance Arbitrary VolumeAction where
  arbitrary Attach {..}       = Attach <$> arbitrary <*> arbitrary <*> arbitrary
  arbitrary Detach {..}       = Detach <$> arbitrary <*> arbitrary <*> arbitrary
  arbitrary Resize {..}       = Resize <$> arbitrary <*> arbitrary <*> arbitrary
  arbitrary AttachByName {..} = AttachByName <$> arbitrary <*> arbitrary <*> arbitrary
  arbitrary DetachByName {..} = DetachByName <$> arbitrary <*> arbitrary <*> arbitrary

prop_VolumeAction = prop_Response (Proxy :: Proxy VolumeAction) -- this should be fetch
-----------------------------------------------------------------

runTests = quickCheck . withMaxSuccess 100 $ prop_Account
                                         .&&. prop_Action
                                         .&&. prop_ActionResponse
                                         .&&. prop_ActionsResponse
                                         .&&. prop_Certificate
                                         .&&. prop_CertificateResponse
                                         .&&. prop_CertificatesResponse
                                         .&&. prop_Region
                                         .&&. prop_RegionsResponse
                                         .&&. prop_Snapshot
                                         .&&. prop_SnapshotResponse
                                         .&&. prop_SnapshotsResponse
                                         .&&. prop_Volume
                                         .&&. prop_VolumeResponse
                                         .&&. prop_VolumesResponse

main :: IO ()
main = return ()
