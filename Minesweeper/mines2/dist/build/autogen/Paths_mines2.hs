module Paths_mines2 (
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

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/x86_64-linux-ghc-7.10.3/mines2-0.0.1-FzLWUp3uF2u2P4XuyIynyU"
datadir    = "/root/.cabal/share/x86_64-linux-ghc-7.10.3/mines2-0.0.1"
libexecdir = "/root/.cabal/libexec"
sysconfdir = "/root/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mines2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mines2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mines2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mines2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mines2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
