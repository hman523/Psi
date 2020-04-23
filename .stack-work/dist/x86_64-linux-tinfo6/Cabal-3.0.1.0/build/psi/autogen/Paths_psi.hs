{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_psi (
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

bindir     = "/home/hunter/Documents/programming/haskell/psi/.stack-work/install/x86_64-linux-tinfo6/825bd46355911b211335a32a466673ca809ccc9a4413babd27d6bb5dd5b4fe60/8.8.3/bin"
libdir     = "/home/hunter/Documents/programming/haskell/psi/.stack-work/install/x86_64-linux-tinfo6/825bd46355911b211335a32a466673ca809ccc9a4413babd27d6bb5dd5b4fe60/8.8.3/lib/x86_64-linux-ghc-8.8.3/psi-0.1.0.0-8a5mbAD0CumKPaGUcOGKd5-psi"
dynlibdir  = "/home/hunter/Documents/programming/haskell/psi/.stack-work/install/x86_64-linux-tinfo6/825bd46355911b211335a32a466673ca809ccc9a4413babd27d6bb5dd5b4fe60/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/hunter/Documents/programming/haskell/psi/.stack-work/install/x86_64-linux-tinfo6/825bd46355911b211335a32a466673ca809ccc9a4413babd27d6bb5dd5b4fe60/8.8.3/share/x86_64-linux-ghc-8.8.3/psi-0.1.0.0"
libexecdir = "/home/hunter/Documents/programming/haskell/psi/.stack-work/install/x86_64-linux-tinfo6/825bd46355911b211335a32a466673ca809ccc9a4413babd27d6bb5dd5b4fe60/8.8.3/libexec/x86_64-linux-ghc-8.8.3/psi-0.1.0.0"
sysconfdir = "/home/hunter/Documents/programming/haskell/psi/.stack-work/install/x86_64-linux-tinfo6/825bd46355911b211335a32a466673ca809ccc9a4413babd27d6bb5dd5b4fe60/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "psi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "psi_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "psi_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "psi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "psi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "psi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
