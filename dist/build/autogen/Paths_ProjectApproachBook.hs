module Paths_ProjectApproachBook (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/edward/Library/Haskell/bin"
libdir     = "/Users/edward/Library/Haskell/ghc-7.8.4-x86_64/lib/ProjectApproachBook-0.0.1"
datadir    = "/Users/edward/Library/Haskell/share/ghc-7.8.4-x86_64/ProjectApproachBook-0.0.1"
libexecdir = "/Users/edward/Library/Haskell/libexec"
sysconfdir = "/Users/edward/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProjectApproachBook_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProjectApproachBook_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ProjectApproachBook_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProjectApproachBook_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProjectApproachBook_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
