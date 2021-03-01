{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pong (
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

bindir     = "/home/butros/.cabal/bin"
libdir     = "/home/butros/.cabal/lib/x86_64-linux-ghc-8.6.5/pong-0.1.0.0-16Km4v1NCq76Va35BmH5M5"
dynlibdir  = "/home/butros/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/butros/.cabal/share/x86_64-linux-ghc-8.6.5/pong-0.1.0.0"
libexecdir = "/home/butros/.cabal/libexec/x86_64-linux-ghc-8.6.5/pong-0.1.0.0"
sysconfdir = "/home/butros/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pong_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pong_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pong_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pong_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pong_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pong_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
