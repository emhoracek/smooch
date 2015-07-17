module Paths_smooch (
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
version = Version [0,0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/libby/.cabal/bin"
libdir     = "/home/libby/.cabal/lib/i386-linux-ghc-7.8.4/smooch-0.0.0.1"
datadir    = "/home/libby/.cabal/share/i386-linux-ghc-7.8.4/smooch-0.0.0.1"
libexecdir = "/home/libby/.cabal/libexec"
sysconfdir = "/home/libby/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "smooch_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "smooch_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "smooch_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "smooch_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "smooch_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
