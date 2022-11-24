{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_nix_managed (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/eyjafjallajokull/Desktop/book/.stack-work/install/x86_64-linux/7c7076413fd40639f98b26d3a654a285d1288d4ca0da4d7cb1951ce054feceff/9.2.4/bin"
libdir     = "/home/eyjafjallajokull/Desktop/book/.stack-work/install/x86_64-linux/7c7076413fd40639f98b26d3a654a285d1288d4ca0da4d7cb1951ce054feceff/9.2.4/lib/x86_64-linux-ghc-9.2.4/nix-managed-0.0.0-2mhh8c0sTbqEx3og9z30Ra-Chapter1"
dynlibdir  = "/home/eyjafjallajokull/Desktop/book/.stack-work/install/x86_64-linux/7c7076413fd40639f98b26d3a654a285d1288d4ca0da4d7cb1951ce054feceff/9.2.4/lib/x86_64-linux-ghc-9.2.4"
datadir    = "/home/eyjafjallajokull/Desktop/book/.stack-work/install/x86_64-linux/7c7076413fd40639f98b26d3a654a285d1288d4ca0da4d7cb1951ce054feceff/9.2.4/share/x86_64-linux-ghc-9.2.4/nix-managed-0.0.0"
libexecdir = "/home/eyjafjallajokull/Desktop/book/.stack-work/install/x86_64-linux/7c7076413fd40639f98b26d3a654a285d1288d4ca0da4d7cb1951ce054feceff/9.2.4/libexec/x86_64-linux-ghc-9.2.4/nix-managed-0.0.0"
sysconfdir = "/home/eyjafjallajokull/Desktop/book/.stack-work/install/x86_64-linux/7c7076413fd40639f98b26d3a654a285d1288d4ca0da4d7cb1951ce054feceff/9.2.4/etc"

getBinDir     = catchIO (getEnv "nix_managed_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "nix_managed_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "nix_managed_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "nix_managed_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "nix_managed_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "nix_managed_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
