module Paths_cp950 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Program Files\\Haskell\\bin"
libdir     = "C:\\Program Files\\Haskell\\cp950-0.1\\ghc-7.4.2"
datadir    = "C:\\Program Files\\Haskell\\cp950-0.1"
libexecdir = "C:\\Program Files\\Haskell\\cp950-0.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "cp950_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cp950_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cp950_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cp950_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
