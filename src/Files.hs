module Files (getAppDir, getCustomStackDir, getStackDir, writeItem) where

import System.EasyFile
import System.IO

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

writeItem :: FilePath -> String -> IO ()
writeItem path contents = do
    let file = joinPath [path, "item"]
    handle <- openFile file WriteMode
    hPutStrLn handle contents
