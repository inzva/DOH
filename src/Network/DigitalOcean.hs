{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Network.DigitalOcean where

-----------------------------------------------------------------
import           Data.Maybe                (maybe)
import           Data.Monoid               ((<>))
import           Control.Lens
import           Data.List                 (intercalate)
import           Data.Proxy
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Bool                 (bool)
import qualified Data.Set                  as Set
-----------------------------------------------------------------
import           Network.DigitalOcean.Types
import           Network.DigitalOcean.Http
import           Network.DigitalOcean.Utils.Pagination
import           Network.DigitalOcean.Services
-----------------------------------------------------------------

getAccounts :: DO Account
getAccounts = unResponse <$> get (Proxy :: Proxy (Response Account)) AccountEndpoint Nothing

getActions :: Maybe PaginationConfig -> DO [Action]
getActions config = getPaginated (Proxy :: Proxy Action) config ActionsEndpoint Nothing

getAction :: ActionId -> DO Action
getAction id' =
  unResponse <$> get (Proxy :: Proxy (Response Action)) (ActionEndpoint id') Nothing

getRegions :: DO [Region]
getRegions =
  unResponse <$> get (Proxy :: Proxy (Response [Region])) RegionsEndpoint Nothing

getVolumes :: DO [Volume]
getVolumes =
  unResponse <$> get (Proxy :: Proxy (Response [Volume])) VolumesEndpoint Nothing

getVolume :: VolumeId -> DO Volume
getVolume id' =
  unResponse <$> get (Proxy :: Proxy (Response Volume)) (VolumeEndpoint id') Nothing

createVolume :: VolumePayload -> DO Volume
createVolume =
  fmap unResponse . post (Proxy :: Proxy (Response Volume)) VolumesEndpoint Nothing

getVolumesByName :: String -> String -> DO [Volume]
getVolumesByName region name =
  let queryParams = Just [("name", name), ("region", region)] in
  unResponse <$> get (Proxy :: Proxy (Response [Volume])) VolumesEndpoint queryParams

data ResourceType = VolumeResource
                  | DropletResource

instance Show ResourceType where
  show VolumeResource = "volume"
  show DropletResource = "droplet"

getSnapshots :: Maybe ResourceType -> DO [Snapshot]
getSnapshots resourceType = do
  let queryParams = ((:[]) . ("resource_type",) . show) <$> resourceType
  unResponse <$> get (Proxy :: Proxy (Response [Snapshot])) SnapshotsEndpoint queryParams

getSnapshot :: SnapshotId -> DO Snapshot
getSnapshot id' =
  unResponse <$> get (Proxy :: Proxy (Response Snapshot)) (SnapshotEndpoint id') Nothing

deleteSnapshot :: SnapshotId -> DO ()
deleteSnapshot id' =
  delete (SnapshotEndpoint id') Nothing

getSnapshotsOfVolume :: VolumeId -> DO [Snapshot]
getSnapshotsOfVolume volumeId =
  unResponse <$> get (Proxy :: Proxy (Response [Snapshot])) (VolumeSnapshotsEndpoint volumeId) Nothing

createSnapshotOfVolume :: VolumeId -> SnapshotPayload -> DO Snapshot
createSnapshotOfVolume volumeId =
  fmap unResponse . post (Proxy :: Proxy (Response Snapshot)) (VolumeSnapshotsEndpoint volumeId) Nothing

deleteVolume :: VolumeId -> DO ()
deleteVolume id' =
  delete (VolumeEndpoint id') Nothing

deleteVolumeByName :: String -> String -> DO ()
deleteVolumeByName region name =
  delete VolumesEndpoint $ Just [("name", name), ("region", region)]

performSingleVolumeAction :: VolumeId -> VolumeAction -> DO Action
performSingleVolumeAction volumeId action =
  unResponse <$> post (Proxy :: Proxy (Response Action)) (VolumeActionsEndpoint volumeId) Nothing action

performListVolumeAction :: VolumeAction -> DO Action
performListVolumeAction action =
  unResponse <$> post (Proxy :: Proxy (Response Action)) VolumesActionsEndpoint Nothing action

performVolumeAction :: VolumeAction -> DO Action
performVolumeAction action@(Attach volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@(Detach volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@(Resize volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@AttachByName {}       = performListVolumeAction action
performVolumeAction action@DetachByName {}       = performListVolumeAction action

getVolumeActions :: VolumeId -> DO [Action]
getVolumeActions volumeId =
  unResponse <$> get (Proxy :: Proxy (Response [Action])) (VolumeActionsEndpoint volumeId) Nothing

getVolumeAction :: VolumeId -> ActionId -> DO Action
getVolumeAction volumeId actionId =
  unResponse <$> get (Proxy :: Proxy (Response Action)) (VolumeActionEndpoint volumeId actionId) Nothing

createCertificate :: Certificatepayload -> DO Certificate
createCertificate = fmap unResponse . post (Proxy :: Proxy (Response Certificate)) CertificatesEndpoint Nothing

getCertificate :: CertificateId -> DO Certificate
getCertificate id' =
  unResponse <$> get (Proxy :: Proxy (Response Certificate)) (CertificateEndpoint id') Nothing

getCertificates :: Maybe PaginationConfig -> DO [Certificate]
getCertificates config = getPaginated (Proxy :: Proxy Certificate) config CertificatesEndpoint Nothing

deleteCertificate :: CertificateId -> DO ()
deleteCertificate id' = delete (CertificateEndpoint id') Nothing

getDomains :: DO [Domain]
getDomains =
  unResponse <$> get (Proxy :: Proxy (Response [Domain])) DomainsEndpoint Nothing

getDomain :: DomainName -> DO Domain
getDomain name' =
  unResponse <$> get (Proxy :: Proxy (Response Domain)) (DomainEndpoint name') Nothing

createDomain :: DomainPayload -> DO Domain
createDomain = fmap unResponse . post (Proxy :: Proxy (Response Domain)) DomainsEndpoint Nothing

deleteDomain :: DomainName -> DO ()
deleteDomain name' = delete (DomainEndpoint name') Nothing

getDomainRecords :: DomainName -> DO [DomainRecord]
getDomainRecords domainName' =
  unResponse <$> get (Proxy :: Proxy (Response [DomainRecord])) (DomainRecordsEndpoint domainName') Nothing

createDomainRecord :: DomainName -> DomainRecordPayload -> DO DomainRecord
createDomainRecord domainName' =
  fmap unResponse . post (Proxy :: Proxy (Response DomainRecord)) (DomainRecordsEndpoint domainName') Nothing

getDomainRecord :: DomainName -> DomainRecordId -> DO DomainRecord
getDomainRecord dn' drid' =
  unResponse <$> get (Proxy :: Proxy (Response DomainRecord)) (DomainRecordEndpoint dn' drid') Nothing

updateDomainRecord :: DomainName -> DomainRecordId -> DomainRecordPayload -> DO DomainRecord
updateDomainRecord dn' drid' =
  fmap unResponse . put (Proxy :: Proxy (Response DomainRecord)) (DomainRecordEndpoint dn' drid') Nothing

deleteDomainRecord :: DomainName -> DomainRecordId -> DO ()
deleteDomainRecord dn' drid' =
  delete (DomainRecordEndpoint dn' drid') Nothing

getImages :: Maybe PaginationConfig -> ImageOptions -> DO [Image]
getImages config ImageOptions {..} =
  getPaginated (Proxy :: Proxy Image) config ImagesEndpoint (Just queryParams)
  where
    queryParams = 
      maybe [] ((:[]) . ("type",) . show) imageType' ++
      bool [] [("private", "true")] isPrivate
