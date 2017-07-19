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
import           Data.Bool                 (bool)
-----------------------------------------------------------------
import           Network.DigitalOcean.Types
import           Network.DigitalOcean.Http
import           Network.DigitalOcean.Utils.Pagination
import           Network.DigitalOcean.Services
-----------------------------------------------------------------

getAccounts :: DO Account
getAccounts = unResponse <$> get (Proxy :: Proxy (Response Account)) "/account" Nothing

getActions :: Maybe PaginationConfig -> DO [Action]
getActions config = getPaginated (Proxy :: Proxy Action) config "/actions"

getAction :: Int -> DO Action
getAction id' =
  unResponse <$> get (Proxy :: Proxy (Response Action)) ("/actions/" ++ show id') Nothing

getRegions :: DO [Region]
getRegions =
  unResponse <$> get (Proxy :: Proxy (Response [Region])) "/regions" Nothing

getVolumes :: DO [Volume]
getVolumes =
  unResponse <$> get (Proxy :: Proxy (Response [Volume])) "/volumes" Nothing

getVolume :: Int -> DO Volume
getVolume id' =
  unResponse <$> get (Proxy :: Proxy (Response Volume)) ("/volumes" <> show id') Nothing

createVolume :: VolumePayload -> DO Volume
createVolume =
  fmap unResponse . post (Proxy :: Proxy (Response Volume)) "/volumes" Nothing

getVolumesByName :: String -> String -> DO [Volume]
getVolumesByName region name =
  let queryParams = Just $ QueryParams [("region", region), ("name", name)] in
  unResponse <$> get (Proxy :: Proxy (Response [Volume])) "/volumes" queryParams

data ResourceType = VolumeResource
                  | DropletResource

instance Show ResourceType where
  show VolumeResource = "volume"
  show DropletResource = "droplet"

getSnapshots :: Maybe ResourceType -> DO [Snapshot]
getSnapshots resourceType = do
  let queryParams = maybe Nothing (\res -> Just $ QueryParams [("resource_type", show res)]) resourceType
  unResponse <$> get (Proxy :: Proxy (Response [Snapshot])) "/snapshots" queryParams

getSnapshot :: SnapshotId -> DO Snapshot
getSnapshot id' =
  unResponse <$> get (Proxy :: Proxy (Response Snapshot)) ("/snapshots/" <> show id') Nothing

deleteSnapshot :: SnapshotId -> DO ()
deleteSnapshot id' =
  delete ("/snapshots/" <> show id') Nothing

getSnapshotsOfVolume :: VolumeId -> DO [Snapshot]
getSnapshotsOfVolume volumeId =
  unResponse <$> get (Proxy :: Proxy (Response [Snapshot])) ("/volumes/" ++ volumeId ++ "/snapshots") Nothing

createSnapshotOfVolume :: VolumeId -> SnapshotPayload -> DO Snapshot
createSnapshotOfVolume volumeId =
  fmap unResponse . post (Proxy :: Proxy (Response Snapshot)) ("/volumes/" ++ volumeId ++ "/snapshots") Nothing

deleteVolume :: VolumeId -> DO ()
deleteVolume id' =
  delete ("/volumes/" <> show id') Nothing

deleteVolumeByName :: String -> String -> DO ()
deleteVolumeByName region name =
  delete "/volumes" . Just $ QueryParams [("region", region), ("name", name)]

performSingleVolumeAction :: VolumeId -> VolumeAction -> DO Action
performSingleVolumeAction volumeId action =
  unResponse <$> post (Proxy :: Proxy (Response Action)) ("/volumes/" <> show volumeId <> "/actions") Nothing action

performListVolumeAction :: VolumeAction -> DO Action
performListVolumeAction action =
  unResponse <$> post (Proxy :: Proxy (Response Action)) "/volumes/actions" Nothing action

performVolumeAction :: VolumeAction -> DO Action
performVolumeAction action@(Attach volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@(Detach volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@(Resize volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@AttachByName {}       = performListVolumeAction action
performVolumeAction action@DetachByName {}       = performListVolumeAction action

getVolumeActions :: VolumeId -> DO [Action]
getVolumeActions volumeId =
  unResponse <$> get (Proxy :: Proxy (Response [Action])) ("/volumes/" <> volumeId <> "/actions") Nothing

getVolumeAction :: VolumeId -> ActionId -> DO Action
getVolumeAction volumeId actionId =
  unResponse <$> get (Proxy :: Proxy (Response Action)) ("/volumes/" <> volumeId <> "/actions/" <> show actionId) Nothing
