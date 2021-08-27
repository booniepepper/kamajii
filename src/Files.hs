module Files(appDir, makeDir) where

import System.EasyFile (createDirectoryIfMissing, getAppUserDataDirectory)

appDir = getAppUserDataDirectory "kamajii"

makeDir = createDirectoryIfMissing True

