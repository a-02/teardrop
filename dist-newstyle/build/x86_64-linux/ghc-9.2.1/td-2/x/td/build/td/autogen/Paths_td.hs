{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_td (
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
version = Version [2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/nks/.cabal/bin"
libdir     = "/home/nks/.cabal/lib/x86_64-linux-ghc-9.2.1/td-2-inplace-td"
dynlibdir  = "/home/nks/.cabal/lib/x86_64-linux-ghc-9.2.1"
datadir    = "/home/nks/.cabal/share/x86_64-linux-ghc-9.2.1/td-2"
libexecdir = "/home/nks/.cabal/libexec/x86_64-linux-ghc-9.2.1/td-2"
sysconfdir = "/home/nks/.cabal/etc"

getBinDir     = catchIO (getEnv "td_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "td_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "td_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "td_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "td_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "td_sysconfdir") (\_ -> return sysconfdir)




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
