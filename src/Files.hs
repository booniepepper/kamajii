module Files (getAppDir, getStackDir, makeDir) where

import System.EasyFile

getAppDir :: IO FilePath
getAppDir = getAppUserDataDirectory "kamajii"

getStackDir :: String -> IO FilePath
getStackDir stackName = do
    app <- getAppDir
    let stack = joinPath [app, stackName]
    makeDir stack
    return stack

makeDir :: FilePath -> IO ()
makeDir = createDirectoryIfMissing True
