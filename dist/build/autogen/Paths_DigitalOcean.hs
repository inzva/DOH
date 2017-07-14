{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_DigitalOcean (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/yigitozkavci/Library/Haskell/bin"
libdir     = "/Users/yigitozkavci/Library/Haskell/ghc-8.0.2-x86_64/lib/DigitalOcean-0.1.0.0"
dynlibdir  = "/Users/yigitozkavci/Library/Haskell/ghc-8.0.2-x86_64/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/yigitozkavci/Library/Haskell/share/ghc-8.0.2-x86_64/DigitalOcean-0.1.0.0"
libexecdir = "/Users/yigitozkavci/Library/Haskell/libexec"
sysconfdir = "/Users/yigitozkavci/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DigitalOcean_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DigitalOcean_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "DigitalOcean_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "DigitalOcean_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DigitalOcean_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DigitalOcean_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
