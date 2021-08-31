module Files (appDir, makeAppDir) where

import System.EasyFile

appDir :: IO FilePath
appDir = getAppUserDataDirectory "kamajii"

makeDir :: FilePath -> IO ()
makeDir = createDirectoryIfMissing True

makeAppDir :: IO ()
makeAppDir = appDir >>= makeDir
