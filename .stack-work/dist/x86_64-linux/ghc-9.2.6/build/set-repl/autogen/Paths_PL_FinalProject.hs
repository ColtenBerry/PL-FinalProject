{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_PL_FinalProject (
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
bindir     = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/9a43702fbcd88bf4a1ed30d4102889a842557e8444eac93e1d9eafb338208a71/9.2.6/bin"
libdir     = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/9a43702fbcd88bf4a1ed30d4102889a842557e8444eac93e1d9eafb338208a71/9.2.6/lib/x86_64-linux-ghc-9.2.6/PL-FinalProject-0.1.0.0-LKpwXaU4QoMISfM6qJXLni-set-repl"
dynlibdir  = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/9a43702fbcd88bf4a1ed30d4102889a842557e8444eac93e1d9eafb338208a71/9.2.6/lib/x86_64-linux-ghc-9.2.6"
datadir    = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/9a43702fbcd88bf4a1ed30d4102889a842557e8444eac93e1d9eafb338208a71/9.2.6/share/x86_64-linux-ghc-9.2.6/PL-FinalProject-0.1.0.0"
libexecdir = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/9a43702fbcd88bf4a1ed30d4102889a842557e8444eac93e1d9eafb338208a71/9.2.6/libexec/x86_64-linux-ghc-9.2.6/PL-FinalProject-0.1.0.0"
sysconfdir = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/9a43702fbcd88bf4a1ed30d4102889a842557e8444eac93e1d9eafb338208a71/9.2.6/etc"

getBinDir     = catchIO (getEnv "PL_FinalProject_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "PL_FinalProject_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "PL_FinalProject_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "PL_FinalProject_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PL_FinalProject_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PL_FinalProject_sysconfdir") (\_ -> return sysconfdir)




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
