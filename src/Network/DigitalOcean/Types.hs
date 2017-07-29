{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Network.DigitalOcean.Types where

-----------------------------------------------------------------
import           GHC.Generics
import           Data.Aeson
import           Data.Time.Clock
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Text                 as T
import qualified Data.ByteString           as BS
import           System.FilePath.Posix     ((</>))
-----------------------------------------------------------------
import           Data.Monoid
import           Data.List
import qualified Data.Set                  as Set
-----------------------------------------------------------------

type QueryParams = [(String, String)]

data RequestMethod =
    Get
  | Post
  | Put
  | Delete

instance Show RequestMethod where
  show Get  = "GET"
  show Post = "POST"
  show Put  = "PUT"
  show Delete  = "DELETE"

showQueryParams :: QueryParams -> String
showQueryParams = \case
  [] -> ""
  ls -> "?" <> (intercalate "&" . map (\(k, v) -> k <> "=" <> v) $ ls)

class (FromJSON a, Show a, FromJSON (PaginationState a)) => Paginatable a where

data Paginatable a => PaginationState a = PaginationState
  { curr     :: [a]
  , page     :: Int
  , nextUrl  :: Maybe String
  , total    :: Maybe Int
  , isLast   :: Bool
  } deriving (Show)

data PaginationConfig = PaginationConfig
  { pageSize :: Int
  , resultLimit :: Int
  }

-- https://developers.digitalocean.com/documentation/v2/#links
defaultPaginationConfig :: PaginationConfig
defaultPaginationConfig = PaginationConfig 25 100

newtype DO a = DO { runDO :: ReaderT Client (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader Client)

newtype Client = Client { apiKey :: BS.ByteString }

type DoErr = T.Text

newtype Response a = Response { unResponse :: a } deriving (Generic, Show)

class (ToJSON a) => Payload a 

data EmptyPayload = EmptyPayload
instance Payload EmptyPayload where
instance ToJSON EmptyPayload where
  toJSON EmptyPayload = object []

instance FromJSON (Response ()) where
  parseJSON _ = return $ Response ()

data Endpoint =
    AccountEndpoint
  | ActionsEndpoint
  | ActionEndpoint ActionId
  | RegionsEndpoint
  | VolumesEndpoint
  | SnapshotsEndpoint
  | DomainsEndpoint
  | ImagesEndpoint
  | SizesEndpoint
  | DropletsEndpoint
  | RecordsEndpoint
  | KernelsEndpoint
  | BackupsEndpoint
  | NeighborsEndpoint
  | FloatingIpsEndpoint
  | DropletsNeighborsEndpoint
  | TagsEndpoint
  | RulesEndpoint
  | FirewallsEndpoint
  | VolumeEndpoint VolumeId
  | SnapshotEndpoint SnapshotId
  | VolumeSnapshotsEndpoint VolumeId
  | VolumesActionsEndpoint
  | VolumeActionEndpoint VolumeId ActionId
  | VolumeActionsEndpoint VolumeId
  | CertificateEndpoint CertificateId
  | CertificatesEndpoint
  | DomainEndpoint DomainName
  | DomainRecordsEndpoint DomainName
  | DomainRecordEndpoint DomainName DomainRecordId
  | ImageActionsEndpoint ImageId
  | ImageEndpoint ImageId
  | ImageBySlugEndpoint String
  | DropletEndpoint DropletId
  | DropletKernelsEndpoint DropletId
  | DropletSnapshotsEndpoint DropletId
  | DropletBackupsEndpoint DropletId
  | DropletActionsEndpoint DropletId
  | DropletNeighborsEndpoint DropletId
  | DropletsActionsEndpoint
  | DropletActionEndpoint DropletId ActionId
  | FloatingIpEndpoint IpAddress
  | FloatingIpActionsEndpoint IpAddress
  | FloatingIpActionEndpoint IpAddress ActionId
  | FirewallEndpoint FirewallId
  | FirewallDropletsEndpoint FirewallId
  | FirewallTagsEndpoint FirewallId
  | FirewallRulesEndpoint FirewallId

instance Show Endpoint where
  show AccountEndpoint                       = "account"
  show ActionsEndpoint                       = "actions"
  show RegionsEndpoint                       = "regions"
  show VolumesEndpoint                       = "volumes"
  show SnapshotsEndpoint                     = "snapshots"
  show CertificatesEndpoint                  = "certificates"
  show DomainsEndpoint                       = "domains"
  show ImagesEndpoint                        = "images"
  show SizesEndpoint                         = "sizes"
  show DropletsEndpoint                      = "droplets"
  show RecordsEndpoint                       = "records"
  show KernelsEndpoint                       = "kernels"
  show BackupsEndpoint                       = "backups"
  show NeighborsEndpoint                     = "neighbors"
  show DropletsNeighborsEndpoint             = "reports/droplet_neighbors"
  show FloatingIpsEndpoint                   = "floating_ips"
  show TagsEndpoint                          = "tags"
  show RulesEndpoint                         = "rules"
  show FirewallsEndpoint                     = "firewalls"
  show (ActionEndpoint id')                  = show ActionsEndpoint        </> show id'
  show (VolumeEndpoint id')                  = show VolumesEndpoint        </> id'
  show (SnapshotEndpoint id')                = show SnapshotsEndpoint      </> id'
  show (VolumeSnapshotsEndpoint id')         = show VolumesEndpoint        </> id'                    </> show SnapshotsEndpoint
  show VolumesActionsEndpoint                = show VolumesEndpoint        </> show SnapshotsEndpoint
  show (VolumeActionsEndpoint vId)           = show VolumesEndpoint        </> vId                    </> show ActionsEndpoint
  show (VolumeActionEndpoint vId aId)        = show VolumesEndpoint        </> vId                    </> show ActionsEndpoint   </> show aId
  show (CertificateEndpoint id')             = show CertificatesEndpoint   </> id'
  show (DomainEndpoint name')                = show DomainsEndpoint        </> name'
  show (DomainRecordsEndpoint name')         = show (DomainEndpoint name') </> show RecordsEndpoint
  show (DomainRecordEndpoint d' dr')         = show (DomainEndpoint d')    </> show RecordsEndpoint   </> show dr'
  show (ImageActionsEndpoint id')            = show ImagesEndpoint         </> show id'               </> show ActionsEndpoint
  show (ImageEndpoint id')                   = show ImagesEndpoint         </> show id'
  show (ImageBySlugEndpoint name')           = show ImagesEndpoint         </> name'
  show (DropletEndpoint id')                 = show DropletsEndpoint       </> show id'
  show (DropletKernelsEndpoint id')          = show DropletsEndpoint       </> show id'               </> show KernelsEndpoint
  show (DropletSnapshotsEndpoint id')        = show DropletsEndpoint       </> show id'               </> show SnapshotsEndpoint
  show (DropletBackupsEndpoint id')          = show DropletsEndpoint       </> show id'               </> show BackupsEndpoint
  show (DropletActionsEndpoint id')          = show DropletsEndpoint       </> show id'               </> show ActionsEndpoint
  show (DropletNeighborsEndpoint id')        = show DropletsEndpoint       </> show id'               </> show NeighborsEndpoint
  show DropletsActionsEndpoint               = show DropletsEndpoint       </> show ActionsEndpoint
  show (DropletActionEndpoint dId aId)       = show DropletsEndpoint       </> show dId               </> show ActionsEndpoint   </> show aId
  show (FloatingIpEndpoint ipAddr)           = show FloatingIpsEndpoint    </> ipAddr
  show (FloatingIpActionsEndpoint ipAddr)    = show FloatingIpsEndpoint    </> ipAddr                 </> show ActionsEndpoint
  show (FloatingIpActionEndpoint ipAddr aId) = show FloatingIpsEndpoint    </> ipAddr                 </> show ActionsEndpoint   </> show aId
  show (FirewallEndpoint id')                = show FirewallsEndpoint      </> id'
  show (FirewallDropletsEndpoint id')        = show FirewallsEndpoint      </> id'                    </> show DropletsEndpoint
  show (FirewallTagsEndpoint id')            = show FirewallsEndpoint      </> id'                    </> show TagsEndpoint
  show (FirewallRulesEndpoint id')           = show FirewallsEndpoint      </> id'                    </> show RulesEndpoint

type VolumeId       = String
type ActionId       = Int
type CertificateId  = String
type SnapshotId     = String
type DomainName     = String
type DomainRecordId = Int
type DropletId      = Int
type RegionSlug     = String
type ImageId        = Int
type DropletName    = String
type IpAddress      = String
type FirewallId     = String
