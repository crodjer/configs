module Paths_xmonad_config (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/rohan/.cabal.sandboxes/xmonad/.cabal-sandbox/bin"
libdir     = "/home/rohan/.cabal.sandboxes/xmonad/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/xmonad-config-0.1.0.0"
datadir    = "/home/rohan/.cabal.sandboxes/xmonad/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/xmonad-config-0.1.0.0"
libexecdir = "/home/rohan/.cabal.sandboxes/xmonad/.cabal-sandbox/libexec"
sysconfdir = "/home/rohan/.cabal.sandboxes/xmonad/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonad_config_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonad_config_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "xmonad_config_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_config_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_config_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
