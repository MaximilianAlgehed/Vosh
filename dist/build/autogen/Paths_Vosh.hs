{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Vosh (
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

bindir     = "/home/max/Repos/Research/Vosh/.cabal-sandbox/bin"
libdir     = "/home/max/Repos/Research/Vosh/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.1/Vosh-0.1.0.0-38SCbSFJ1Ye7Hk6Nlwa2TU"
dynlibdir  = "/home/max/Repos/Research/Vosh/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/max/Repos/Research/Vosh/.cabal-sandbox/share/x86_64-linux-ghc-8.0.1/Vosh-0.1.0.0"
libexecdir = "/home/max/Repos/Research/Vosh/.cabal-sandbox/libexec"
sysconfdir = "/home/max/Repos/Research/Vosh/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Vosh_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Vosh_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Vosh_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Vosh_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Vosh_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Vosh_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
