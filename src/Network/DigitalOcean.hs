{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
performVolumeAction action@(ResizeVolume volumeId _ _) = performSingleVolumeAction volumeId action
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

getSizes :: DO [Size]
getSizes = unResponse <$> get SizesEndpoint Nothing

getDroplets :: Maybe PaginationConfig -> DO [Droplet]
getDroplets config = getPaginated config DropletsEndpoint Nothing

createDroplet :: DropletName -> IDropletPayload -> DO Droplet
createDroplet name payload = unResponse <$> post DropletsEndpoint Nothing (SingleDropletPayload name payload)

createDroplets :: [DropletName] -> IDropletPayload -> DO [Droplet]
createDroplets names payload = unResponse <$> post DropletsEndpoint Nothing (MultipleDropletPayload names payload)

getDroplet :: DropletId -> DO Droplet
getDroplet id' = unResponse <$> get (DropletEndpoint id') Nothing

getDropletsByTag :: String -> DO [Droplet]
getDropletsByTag tag = unResponse <$> get DropletsEndpoint (Just [("tag_name", tag)])

getDropletKernels :: DropletId -> DO [Kernel]
getDropletKernels id' = unResponse <$> get (DropletKernelsEndpoint id') Nothing

getDropletSnapshots :: DropletId -> DO [Snapshot]
getDropletSnapshots id' = unResponse <$> get (DropletSnapshotsEndpoint id') Nothing

getDropletBackups :: DropletId -> DO [Backup]
getDropletBackups id' = unResponse <$> get (DropletBackupsEndpoint id') Nothing

getDropletActions :: DropletId -> DO [Action]
getDropletActions id' = unResponse <$> get (DropletActionsEndpoint id') Nothing

deleteDroplet :: DropletId -> DO ()
deleteDroplet id' = delete (DropletEndpoint id') Nothing

deleteDropletByTag :: String -> DO ()
deleteDropletByTag tag = delete DropletsEndpoint $ Just [("tag_name", tag)]

getDropletNeighbors :: DropletId -> DO [Droplet]
getDropletNeighbors id' = unResponse <$> get (DropletNeighborsEndpoint id') Nothing

getNeighbors :: DO Neighbors
getNeighbors = unResponse <$> get DropletsNeighborsEndpoint Nothing

performDropletAction :: DropletId -> DropletAction -> DO Action
performDropletAction id' = fmap unResponse . post (DropletActionsEndpoint id') Nothing 

{- WARNING: Currently has issue with the response format, see:
 - https://github.com/digitalocean/api-v2/issues/164
 -}
performDropletActionOnTag :: String -> DropletAction -> DO [Action]
performDropletActionOnTag tag action = do
  unless (actionAllowedAsBulk action) $
    throwError $
      "Action " ++ show action ++ " not allowed as bulk. See \
      \https://developers.digitalocean.com/documentation/v2/#acting-on-tagged-droplets"
  unResponse <$> post DropletsActionsEndpoint (Just [("tag_name", tag)]) action

getDropletAction :: DropletId -> ActionId -> DO Action
getDropletAction dropletId actionId = unResponse <$> get (DropletActionEndpoint dropletId actionId) Nothing
