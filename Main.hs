{-# LANGUAGE OverloadedStrings #-}

module Main where

-----------------------------------------------------------------
import Network.DigitalOcean
import Network.DigitalOcean.Types
import Network.DigitalOcean.Services
-----------------------------------------------------------------
import Control.Monad.Except
import Control.Monad.Reader
-----------------------------------------------------------------

client :: Client
client = Client "your api key"

main :: IO ()
main = do
  result <- runExceptT $ (runReaderT $ runDO createViaSshKeys) client
  case result of
    Left err -> print err
    Right _ -> return ()

createViaSshKeys :: DO ()
createViaSshKeys = do
  -- Read a public key from a key pair and create ssh keys on DigitalOcean with it
  pubKey <- liftIO $ readFile "/Users/yigitozkavci/.ssh/do_api_rsa.pub"
  sshKey <- createSSHKey (SSHKeyPayload "my api key" pubKey) 
  
  -- Create 2 droplets with our newly uploaded ssh keys
  let dropletPayload = IDropletPayload "nyc3" "512mb" "ubuntu-14-04-x64" (Just [sshkeyFingerprint sshKey]) Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  droplets <- map dropletId <$> createDroplets ["droplet-1", "droplet-2"] dropletPayload

  -- Take snapshot of our newly created droplets
  forM_ droplets $ \dropletId -> performDropletAction dropletId (TakeSnapshot (Just "bulk snapshot"))
