module Files (getAppDir, getCustomStackDir, getStackDir) where

import System.EasyFile

getAppDir :: IO FilePath
getAppDir = getAppUserDataDirectory "kamajii"

getStackDir :: String -> IO FilePath
getStackDir stackName = do
    app <- getAppDir
    getCustomStackDir app stackName

getCustomStackDir :: FilePath -> String -> IO FilePath
getCustomStackDir path stack = do
    let stackPath = joinPath [path, stack]
    makeDir stackPath
    return stackPath

makeDir :: FilePath -> IO ()
makeDir = createDirectoryIfMissing True
