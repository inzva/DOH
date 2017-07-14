{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.DigitalOcean where

------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Text            as T
------------------------------------------------

data Client = Client { apiKey :: T.Text }
type DoErr = T.Text

newtype DO a = DO { runDO :: ReaderT Client (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader Client)
