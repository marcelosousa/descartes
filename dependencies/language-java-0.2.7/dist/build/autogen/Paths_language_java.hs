module Paths_language_java (
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
version = Version [0,2,7] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mabs/Library/Haskell/ghc-7.8.3/lib/language-java-0.2.7/bin"
libdir     = "/Users/mabs/Library/Haskell/ghc-7.8.3/lib/language-java-0.2.7/lib"
datadir    = "/Users/mabs/Library/Haskell/ghc-7.8.3/lib/language-java-0.2.7/share"
libexecdir = "/Users/mabs/Library/Haskell/ghc-7.8.3/lib/language-java-0.2.7/libexec"
sysconfdir = "/Users/mabs/Library/Haskell/ghc-7.8.3/lib/language-java-0.2.7/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "language_java_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "language_java_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "language_java_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "language_java_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "language_java_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
