{-|
Module      : DigitalOcean
Description : This module is where user shall interract with the whole API
Copyright   : (c) Yiğit Özkavcı, 2017
License     : MIT
Maintainer  : yigitozkavci8@gmail.com
Stability   : beta
Portability : POSIX

Every computation within this module should be assessed via `DO` monad.

An example usage:

@
{&#45;\# LANGUAGE OverloadedStrings \#&#45;}

module Main where

import 'Network.DigitalOcean'
import Control.Monad.Except
import Control.Monad.Reader

client :: 'Client'
client = 'Client' \"my api key\"

main :: IO ()
main = do
  result <- runExceptT $ (runReaderT $ \"runDO\" doActions) client
  case result of
    Left err -> print err
    Right _ -> return ()

createViaSshKeys :: 'DO' ()
createViaSshKeys = do
  -- Read a public key from a key pair and create ssh keys on DigitalOcean with it
  pubKey <- liftIO $ readFile \"\/Users\/yigitozkavci\/.ssh\/do_api_rsa.pub\"
  sshKey <- 'createSSHKey' ('SSHKeyPayload' \"my ssh key\" pubKey) 
  
  -- Create 2 droplets with our newly uploaded ssh keys
  let dropletPayload = 'IDropletPayload' \"nyc3\" \"512mb\" \"ubuntu-14-04-x64\" (Just [sshkeyFingerprint sshKey]) Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  droplets <- map dropletId <$\> 'createDroplets' ["droplet-1", "droplet-2"] dropletPayload

  -- Take snapshot of our newly created droplets
  forM_ droplets $ \dropletId -> 'performDropletAction' dropletId ('TakeSnapshot' (Just "bulk snapshot"))
@
-}

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

-- * Account
getAccounts :: DO Account
getAccounts = unResponse <$> get' AccountEndpoint

-- * Actions
getActions :: Maybe PaginationConfig -> DO [Action]
getActions config = getPaginated config ActionsEndpoint Nothing

getAction :: ActionId -> DO Action
getAction id' =
  unResponse <$> get' (ActionEndpoint id')

-- * Regions
getRegions :: DO [Region]
getRegions =
  unResponse <$> get' RegionsEndpoint

-- * Volumes
getVolumes :: DO [Volume]
getVolumes =
  unResponse <$> get' VolumesEndpoint

getVolume :: VolumeId -> DO Volume
getVolume id' =
  unResponse <$> get' (VolumeEndpoint id')

createVolume :: VolumePayload -> DO Volume
createVolume =
  fmap unResponse . post VolumesEndpoint Nothing

getVolumesByName :: String -> String -> DO [Volume]
getVolumesByName region name =
  let queryParams = Just [("name", name), ("region", region)] in
  unResponse <$> get VolumesEndpoint queryParams

deleteVolume :: VolumeId -> DO ()
deleteVolume id' =
  delete' (VolumeEndpoint id')

deleteVolumeByName :: String -> String -> DO ()
deleteVolumeByName region name =
  delete VolumesEndpoint (Just [("name", name), ("region", region)]) EmptyPayload

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
  unResponse <$> get' (VolumeActionsEndpoint volumeId)

getVolumeAction :: VolumeId -> ActionId -> DO Action
getVolumeAction volumeId actionId =
  unResponse <$> get' (VolumeActionEndpoint volumeId actionId)

-- * Snapshots
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
  unResponse <$> get' (SnapshotEndpoint id')

deleteSnapshot :: SnapshotId -> DO ()
deleteSnapshot id' =
  delete' (SnapshotEndpoint id')

getSnapshotsOfVolume :: VolumeId -> DO [Snapshot]
getSnapshotsOfVolume volumeId =
  unResponse <$> get' (VolumeSnapshotsEndpoint volumeId)

createSnapshotOfVolume :: VolumeId -> SnapshotPayload -> DO Snapshot
createSnapshotOfVolume volumeId =
  fmap unResponse . post (VolumeSnapshotsEndpoint volumeId) Nothing

-- * Certificates
createCertificate :: Certificatepayload -> DO Certificate
createCertificate = fmap unResponse . post CertificatesEndpoint Nothing

getCertificate :: CertificateId -> DO Certificate
getCertificate id' =
  unResponse <$> get' (CertificateEndpoint id')

getCertificates :: Maybe PaginationConfig -> DO [Certificate]
getCertificates config = getPaginated config CertificatesEndpoint Nothing

deleteCertificate :: CertificateId -> DO ()
deleteCertificate id' = delete' (CertificateEndpoint id')

-- * Domains
getDomains :: DO [Domain]
getDomains =
  unResponse <$> get' DomainsEndpoint

getDomain :: DomainName -> DO Domain
getDomain name' =
  unResponse <$> get' (DomainEndpoint name')

createDomain :: DomainPayload -> DO Domain
createDomain = fmap unResponse . post DomainsEndpoint Nothing

deleteDomain :: DomainName -> DO ()
deleteDomain name' = delete' (DomainEndpoint name')

-- * Domain Records
getDomainRecords :: DomainName -> DO [DomainRecord]
getDomainRecords domainName' =
  unResponse <$> get' (DomainRecordsEndpoint domainName')

createDomainRecord :: DomainName -> DomainRecordPayload -> DO DomainRecord
createDomainRecord domainName' =
  fmap unResponse . post (DomainRecordsEndpoint domainName') Nothing

getDomainRecord :: DomainName -> DomainRecordId -> DO DomainRecord
getDomainRecord dn' drid' =
  unResponse <$> get' (DomainRecordEndpoint dn' drid')

updateDomainRecord :: DomainName -> DomainRecordId -> DomainRecordPayload -> DO DomainRecord
updateDomainRecord dn' drid' =
  fmap unResponse . put (DomainRecordEndpoint dn' drid') Nothing

deleteDomainRecord :: DomainName -> DomainRecordId -> DO ()
deleteDomainRecord dn' drid' =
  delete' (DomainRecordEndpoint dn' drid')

-- * Images
getImages :: Maybe PaginationConfig -> ImageOptions -> DO [Image]
getImages config ImageOptions {..} =
  getPaginated config ImagesEndpoint (Just queryParams)
  where
    queryParams = 
      maybe [] ((:[]) . ("type",) . show) imageType' ++
      bool [] [("private", "true")] isPrivate

getImageActions :: ImageId -> DO [Action]
getImageActions id' =
  unResponse <$> get' (ImageActionsEndpoint id')

getImageAction :: ImageId -> ActionId -> DO Action
getImageAction imageId actionId = unResponse <$> get' (ImageActionEndpoint imageId actionId)

getImage :: ImageId -> DO [Image]
getImage id' =
  unResponse <$> get' (ImageEndpoint id')

getImageBySlug :: String -> DO [Image]
getImageBySlug slug = unResponse <$> get' (ImageBySlugEndpoint slug)

updateImage :: ImageId -> ImagePayload -> DO Image
updateImage id' = fmap unResponse . put (ImageEndpoint id') Nothing

deleteImage :: ImageId -> DO ()
deleteImage id' = delete' (ImageEndpoint id')

performImageAction :: ImageId -> ImageAction -> DO Action
performImageAction id' = fmap unResponse . post (ImageActionsEndpoint id') Nothing 

-- * Sizes
getSizes :: DO [Size]
getSizes = unResponse <$> get' SizesEndpoint

-- * Droplets
getDroplets :: Maybe PaginationConfig -> DO [Droplet]
getDroplets config = getPaginated config DropletsEndpoint Nothing

createDroplet :: DropletName -> IDropletPayload -> DO Droplet
createDroplet name payload = unResponse <$> post DropletsEndpoint Nothing (SingleDropletPayload name payload)

createDroplets :: [DropletName] -> IDropletPayload -> DO [Droplet]
createDroplets names payload = unResponse <$> post DropletsEndpoint Nothing (MultipleDropletPayload names payload)

getDroplet :: DropletId -> DO Droplet
getDroplet id' = unResponse <$> get' (DropletEndpoint id')

getDropletsByTag :: String -> DO [Droplet]
getDropletsByTag tag = unResponse <$> get DropletsEndpoint (Just [("tag_name", tag)])

getDropletKernels :: DropletId -> DO [Kernel]
getDropletKernels id' = unResponse <$> get' (DropletKernelsEndpoint id')

getDropletSnapshots :: DropletId -> DO [Snapshot]
getDropletSnapshots id' = unResponse <$> get' (DropletSnapshotsEndpoint id')

getDropletBackups :: DropletId -> DO [Backup]
getDropletBackups id' = unResponse <$> get' (DropletBackupsEndpoint id')

getDropletActions :: DropletId -> DO [Action]
getDropletActions id' = unResponse <$> get' (DropletActionsEndpoint id')

deleteDroplet :: DropletId -> DO ()
deleteDroplet id' = delete' (DropletEndpoint id')

deleteDropletByTag :: String -> DO ()
deleteDropletByTag tag = delete DropletsEndpoint (Just [("tag_name", tag)]) EmptyPayload

getDropletNeighbors :: DropletId -> DO [Droplet]
getDropletNeighbors id' = unResponse <$> get' (DropletNeighborsEndpoint id')

getNeighbors :: DO Neighbors
getNeighbors = unResponse <$> get' DropletsNeighborsEndpoint

performDropletAction :: DropletId -> DropletAction -> DO Action
performDropletAction id' = fmap unResponse . post (DropletActionsEndpoint id') Nothing 

{- Warning! Currently has issue with the response format, see:
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
getDropletAction dropletId actionId = unResponse <$> get' (DropletActionEndpoint dropletId actionId)

-- * Floating IPs
getFloatingIps :: DO [FloatingIp]
getFloatingIps = unResponse <$> get' FloatingIpsEndpoint

createFloatingIp :: FloatingIpPayload -> DO FloatingIp
createFloatingIp = fmap unResponse . post FloatingIpsEndpoint Nothing

getFloatingIp :: IpAddress -> DO FloatingIp
getFloatingIp ip = unResponse <$> get' (FloatingIpEndpoint ip)

deleteFloatingIp :: IpAddress -> DO ()
deleteFloatingIp ip = delete' (FloatingIpEndpoint ip)

performFloatingIpAction :: IpAddress -> FloatingIpAction -> DO Action
performFloatingIpAction ip = fmap unResponse . post (FloatingIpActionsEndpoint ip) Nothing

getFloatingIpActions :: IpAddress -> DO [Action]
getFloatingIpActions ip = unResponse <$> get' (FloatingIpActionsEndpoint ip)

getFloatingIpAction :: IpAddress -> ActionId -> DO Action
getFloatingIpAction ip aId = unResponse <$> get' (FloatingIpActionEndpoint ip aId)

-- * Firewalls
createFirewall :: FirewallPayload -> DO Firewall
createFirewall = fmap unResponse . post FirewallsEndpoint Nothing

getFirewall :: FirewallId -> DO Firewall
getFirewall id' = unResponse <$> get' (FirewallEndpoint id')

getFirewalls :: DO [Firewall]
getFirewalls = unResponse <$> get' FirewallsEndpoint

updateFirewall :: FirewallId -> FirewallPayload -> DO Firewall
updateFirewall id' = fmap unResponse . put (FirewallEndpoint id') Nothing

deleteFirewall :: FirewallId -> DO ()
deleteFirewall id' = delete' (FirewallEndpoint id')

addDropletsToFirewall :: FirewallId -> DropletsPayload -> DO ()
addDropletsToFirewall id' = fmap unResponse . post (FirewallDropletsEndpoint id') Nothing

removeDropletsFromFirewall :: FirewallId -> DropletsPayload -> DO ()
removeDropletsFromFirewall id' = delete (FirewallDropletsEndpoint id') Nothing

addTagsToFirewall :: FirewallId -> TagsPayload -> DO ()
addTagsToFirewall id' = fmap unResponse . post (FirewallTagsEndpoint id') Nothing

removeTagsFromFirewall :: FirewallId -> TagsPayload -> DO ()
removeTagsFromFirewall id' = delete (FirewallTagsEndpoint id') Nothing

addRulesToFirewall :: FirewallId -> FirewallRulesPayload -> DO ()
addRulesToFirewall id' = fmap unResponse . post (FirewallRulesEndpoint id') Nothing

removeRulesFromFirewall :: FirewallId -> FirewallRulesPayload -> DO ()
removeRulesFromFirewall id' = delete (FirewallRulesEndpoint id') Nothing

-- * Load Balancers
createLoadBalancer :: LoadBalancerPayload -> DO LoadBalancer
createLoadBalancer = fmap unResponse . post LoadBalancersEndpoint Nothing

getLoadBalancer :: LoadBalancerId -> DO LoadBalancer
getLoadBalancer id' = unResponse <$> get' (LoadBalancerEndpoint id')

getLoadBalancers :: DO [LoadBalancer]
getLoadBalancers = unResponse <$> get' LoadBalancersEndpoint

updateLoadBalancer :: LoadBalancerPayload -> DO LoadBalancer
updateLoadBalancer = fmap unResponse . put LoadBalancersEndpoint Nothing

deleteLoadBalancer :: LoadBalancerId -> DO ()
deleteLoadBalancer id' = delete' (LoadBalancerEndpoint id')

addDropletsToLoadBalancer :: LoadBalancerId -> DropletsPayload -> DO ()
addDropletsToLoadBalancer id' = post (LoadBalancerDropletsEndpoint id') Nothing

removeDropletsFromLoadBalancer :: LoadBalancerId -> DropletsPayload -> DO ()
removeDropletsFromLoadBalancer id' = delete (LoadBalancerDropletsEndpoint id') Nothing

addForwardingRulesToLoadBalancer :: LoadBalancerId -> [ForwardingRule] -> DO ()
addForwardingRulesToLoadBalancer id' = post (LoadBalancerForwardingRulesEndpoint id') Nothing

removeForwardingRulesFromLoadBalancer :: LoadBalancerId -> [ForwardingRule] -> DO ()
removeForwardingRulesFromLoadBalancer id' = delete (LoadBalancerForwardingRulesEndpoint id') Nothing

-- * SSH Keys
getSSHKeys :: DO [SSHKey]
getSSHKeys = unResponse <$> get' SSHKeysEndpoint

createSSHKey :: SSHKeyPayload -> DO SSHKey
createSSHKey = fmap unResponse . post SSHKeysEndpoint Nothing

getSSHKey :: Either SSHKeyId String -> DO SSHKey
getSSHKey idn = get' $ case idn of
  Left id' -> SSHKeyEndpoint id'
  Right fingerprint -> SSHKeyWithFingerprintEndpoint fingerprint

updateSSHKey :: Either SSHKeyId String -> String -> DO SSHKey
updateSSHKey idn name =
  unResponse <$>
    put (either SSHKeyEndpoint SSHKeyWithFingerprintEndpoint idn) Nothing (SSHKeyNamePayload name)

destroySSHKey :: Either SSHKeyId String -> DO ()
destroySSHKey = delete' . either SSHKeyEndpoint SSHKeyWithFingerprintEndpoint
