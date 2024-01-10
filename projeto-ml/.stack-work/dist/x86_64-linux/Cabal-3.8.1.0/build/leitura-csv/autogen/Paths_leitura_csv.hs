{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_leitura_csv (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/pedro/Documents/UFABC/Q2 2023/PP/Projeto/leitura-csv/.stack-work/install/x86_64-linux/9ce6d4b6c2c797c5287659093af62f608bf517752c0b8b256795976d3fbe4631/9.4.5/bin"
libdir     = "/home/pedro/Documents/UFABC/Q2 2023/PP/Projeto/leitura-csv/.stack-work/install/x86_64-linux/9ce6d4b6c2c797c5287659093af62f608bf517752c0b8b256795976d3fbe4631/9.4.5/lib/x86_64-linux-ghc-9.4.5/leitura-csv-0.1.0.0-JosxLkHB19j97EF89Oqxty-leitura-csv"
dynlibdir  = "/home/pedro/Documents/UFABC/Q2 2023/PP/Projeto/leitura-csv/.stack-work/install/x86_64-linux/9ce6d4b6c2c797c5287659093af62f608bf517752c0b8b256795976d3fbe4631/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/pedro/Documents/UFABC/Q2 2023/PP/Projeto/leitura-csv/.stack-work/install/x86_64-linux/9ce6d4b6c2c797c5287659093af62f608bf517752c0b8b256795976d3fbe4631/9.4.5/share/x86_64-linux-ghc-9.4.5/leitura-csv-0.1.0.0"
libexecdir = "/home/pedro/Documents/UFABC/Q2 2023/PP/Projeto/leitura-csv/.stack-work/install/x86_64-linux/9ce6d4b6c2c797c5287659093af62f608bf517752c0b8b256795976d3fbe4631/9.4.5/libexec/x86_64-linux-ghc-9.4.5/leitura-csv-0.1.0.0"
sysconfdir = "/home/pedro/Documents/UFABC/Q2 2023/PP/Projeto/leitura-csv/.stack-work/install/x86_64-linux/9ce6d4b6c2c797c5287659093af62f608bf517752c0b8b256795976d3fbe4631/9.4.5/etc"

getBinDir     = catchIO (getEnv "leitura_csv_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "leitura_csv_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "leitura_csv_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "leitura_csv_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "leitura_csv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "leitura_csv_sysconfdir") (\_ -> return sysconfdir)




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
