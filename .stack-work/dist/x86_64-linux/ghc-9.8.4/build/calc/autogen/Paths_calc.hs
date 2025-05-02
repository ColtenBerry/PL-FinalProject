{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_calc (
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
version = Version [1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/b121180d4c3a13ddb5f62fdcc08ab54f74cc82ab508eef84206bcb78909d7af2/9.8.4/bin"
libdir     = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/b121180d4c3a13ddb5f62fdcc08ab54f74cc82ab508eef84206bcb78909d7af2/9.8.4/lib/x86_64-linux-ghc-9.8.4/calc-1.0-5V7xbVpvMpKFiYDdDiS3Z8-calc"
dynlibdir  = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/b121180d4c3a13ddb5f62fdcc08ab54f74cc82ab508eef84206bcb78909d7af2/9.8.4/lib/x86_64-linux-ghc-9.8.4"
datadir    = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/b121180d4c3a13ddb5f62fdcc08ab54f74cc82ab508eef84206bcb78909d7af2/9.8.4/share/x86_64-linux-ghc-9.8.4/calc-1.0"
libexecdir = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/b121180d4c3a13ddb5f62fdcc08ab54f74cc82ab508eef84206bcb78909d7af2/9.8.4/libexec/x86_64-linux-ghc-9.8.4/calc-1.0"
sysconfdir = "/mnt/c/Users/noahc/OneDrive/Desktop/JuniorYear/SpringSemester2025/ProgrammingLanguages/Project4/PL-FinalProject/.stack-work/install/x86_64-linux/b121180d4c3a13ddb5f62fdcc08ab54f74cc82ab508eef84206bcb78909d7af2/9.8.4/etc"

getBinDir     = catchIO (getEnv "calc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "calc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "calc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "calc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "calc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "calc_sysconfdir") (\_ -> return sysconfdir)



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
