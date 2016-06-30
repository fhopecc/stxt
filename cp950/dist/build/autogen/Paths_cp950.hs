{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_cp950 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\ccl00695.DOMAIN\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\ccl00695.DOMAIN\\AppData\\Roaming\\cabal\\i386-windows-ghc-8.0.1\\cp950-0.1-EUpY5hJHF5kFjz4Hc702Z0"
datadir    = "C:\\Users\\ccl00695.DOMAIN\\AppData\\Roaming\\cabal\\i386-windows-ghc-8.0.1\\cp950-0.1"
libexecdir = "C:\\Users\\ccl00695.DOMAIN\\AppData\\Roaming\\cabal\\cp950-0.1-EUpY5hJHF5kFjz4Hc702Z0"
sysconfdir = "C:\\Users\\ccl00695.DOMAIN\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cp950_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cp950_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cp950_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cp950_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cp950_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
