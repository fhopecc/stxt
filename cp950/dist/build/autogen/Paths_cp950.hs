module Paths_cp950 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Documents and Settings\\ccl00695\\Application Data\\cabal\\bin"
libdir     = "C:\\Documents and Settings\\ccl00695\\Application Data\\cabal\\cp950-0.1\\ghc-7.0.4"
datadir    = "C:\\Documents and Settings\\ccl00695\\Application Data\\cabal\\cp950-0.1"
libexecdir = "C:\\Documents and Settings\\ccl00695\\Application Data\\cabal\\cp950-0.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "cp950_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "cp950_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "cp950_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "cp950_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
