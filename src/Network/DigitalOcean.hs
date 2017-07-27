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
getAccounts = unResponse <$> get AccountEndpoint Nothing

getActions :: Maybe PaginationConfig -> DO [Action]
getActions config = getPaginated config ActionsEndpoint Nothing

getAction :: ActionId -> DO Action
getAction id' =
  unResponse <$> get (ActionEndpoint id') Nothing

getRegions :: DO [Region]
getRegions =
  unResponse <$> get RegionsEndpoint Nothing

getVolumes :: DO [Volume]
getVolumes =
  unResponse <$> get VolumesEndpoint Nothing

getVolume :: VolumeId -> DO Volume
getVolume id' =
  unResponse <$> get (VolumeEndpoint id') Nothing

createVolume :: VolumePayload -> DO Volume
createVolume =
  fmap unResponse . post VolumesEndpoint Nothing

getVolumesByName :: String -> String -> DO [Volume]
getVolumesByName region name =
  let queryParams = Just [("name", name), ("region", region)] in
  unResponse <$> get VolumesEndpoint queryParams

data ResourceType = VolumeResource
                  | DropletResource

instance Show ResourceType where
  show VolumeResource = "volume"
  show DropletResource = "droplet"

getSnapshots :: Maybe ResourceType -> DO [Snapshot]
getSnapshots resourceType = do
  let queryParams = ((:[]) . ("resource_type",) . show) <$> resourceType
  unResponse <$> get SnapshotsEndpoint queryParams

getSnapshot :: SnapshotId -> DO Snapshot
getSnapshot id' =
  unResponse <$> get (SnapshotEndpoint id') Nothing

deleteSnapshot :: SnapshotId -> DO ()
deleteSnapshot id' =
  delete (SnapshotEndpoint id') Nothing

getSnapshotsOfVolume :: VolumeId -> DO [Snapshot]
getSnapshotsOfVolume volumeId =
  unResponse <$> get (VolumeSnapshotsEndpoint volumeId) Nothing

createSnapshotOfVolume :: VolumeId -> SnapshotPayload -> DO Snapshot
createSnapshotOfVolume volumeId =
  fmap unResponse . post (VolumeSnapshotsEndpoint volumeId) Nothing

deleteVolume :: VolumeId -> DO ()
deleteVolume id' =
  delete (VolumeEndpoint id') Nothing

deleteVolumeByName :: String -> String -> DO ()
deleteVolumeByName region name =
  delete VolumesEndpoint $ Just [("name", name), ("region", region)]

performSingleVolumeAction :: VolumeId -> VolumeAction -> DO Action
performSingleVolumeAction volumeId action =
  unResponse <$> post (VolumeActionsEndpoint volumeId) Nothing action

performListVolumeAction :: VolumeAction -> DO Action
performListVolumeAction action =
  unResponse <$> post VolumesActionsEndpoint Nothing action

performVolumeAction :: VolumeAction -> DO Action
performVolumeAction action@(Attach volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@(Detach volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@(Resize volumeId _ _) = performSingleVolumeAction volumeId action
performVolumeAction action@AttachByName {}       = performListVolumeAction action
performVolumeAction action@DetachByName {}       = performListVolumeAction action

getVolumeActions :: VolumeId -> DO [Action]
getVolumeActions volumeId =
  unResponse <$> get (VolumeActionsEndpoint volumeId) Nothing

getVolumeAction :: VolumeId -> ActionId -> DO Action
getVolumeAction volumeId actionId =
  unResponse <$> get (VolumeActionEndpoint volumeId actionId) Nothing

createCertificate :: Certificatepayload -> DO Certificate
createCertificate = fmap unResponse . post CertificatesEndpoint Nothing

getCertificate :: CertificateId -> DO Certificate
getCertificate id' =
  unResponse <$> get (CertificateEndpoint id') Nothing

getCertificates :: Maybe PaginationConfig -> DO [Certificate]
getCertificates config = getPaginated config CertificatesEndpoint Nothing

deleteCertificate :: CertificateId -> DO ()
deleteCertificate id' = delete (CertificateEndpoint id') Nothing

getDomains :: DO [Domain]
getDomains =
  unResponse <$> get DomainsEndpoint Nothing

getDomain :: DomainName -> DO Domain
getDomain name' =
  unResponse <$> get (DomainEndpoint name') Nothing

createDomain :: DomainPayload -> DO Domain
createDomain = fmap unResponse . post DomainsEndpoint Nothing

deleteDomain :: DomainName -> DO ()
deleteDomain name' = delete (DomainEndpoint name') Nothing

getDomainRecords :: DomainName -> DO [DomainRecord]
getDomainRecords domainName' =
  unResponse <$> get (DomainRecordsEndpoint domainName') Nothing

createDomainRecord :: DomainName -> DomainRecordPayload -> DO DomainRecord
createDomainRecord domainName' =
  fmap unResponse . post (DomainRecordsEndpoint domainName') Nothing

getDomainRecord :: DomainName -> DomainRecordId -> DO DomainRecord
getDomainRecord dn' drid' =
  unResponse <$> get (DomainRecordEndpoint dn' drid') Nothing

updateDomainRecord :: DomainName -> DomainRecordId -> DomainRecordPayload -> DO DomainRecord
updateDomainRecord dn' drid' =
  fmap unResponse . put (DomainRecordEndpoint dn' drid') Nothing

deleteDomainRecord :: DomainName -> DomainRecordId -> DO ()
deleteDomainRecord dn' drid' =
  delete (DomainRecordEndpoint dn' drid') Nothing

getImages :: Maybe PaginationConfig -> ImageOptions -> DO [Image]
getImages config ImageOptions {..} =
  getPaginated config ImagesEndpoint (Just queryParams)
  where
    queryParams = 
      maybe [] ((:[]) . ("type",) . show) imageType' ++
      bool [] [("private", "true")] isPrivate

getImageActions :: ImageId -> DO [Action]
getImageActions id' =
  unResponse <$> get (ImageActionsEndpoint id') Nothing

getImage :: ImageId -> DO [Image]
getImage id' =
  unResponse <$> get (ImageEndpoint id') Nothing

getImageBySlug :: String -> DO [Image]
getImageBySlug slug = unResponse <$> get (ImageBySlugEndpoint slug) Nothing

updateImage :: ImageId -> ImagePayload -> DO Image
updateImage id' = fmap unResponse . put (ImageEndpoint id') Nothing

deleteImage :: ImageId -> DO ()
deleteImage id' = delete (ImageEndpoint id') Nothing
