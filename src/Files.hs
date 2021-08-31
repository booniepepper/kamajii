module Files (appDir, makeDir) where

import System.EasyFile

appDir :: IO FilePath
appDir = getAppUserDataDirectory "kamajii"

makeDir :: FilePath -> IO ()
makeDir = createDirectoryIfMissing True
