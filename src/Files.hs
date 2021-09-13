module Files (getAppDir, getCustomStackDir, getStackDir, pushItem) where

import Control.Monad
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

pushItem :: FilePath -> String -> IO ()
pushItem path contents = do
    let nextDir = joinPath [path, "next"]
    nextDirExists <- doesDirectoryExist nextDir
    when nextDirExists $ do
        let tempNext = joinPath [path, "temp-next"]
        let tempNextNext = joinPath [path, "temp-next", "next"]
        makeDir tempNext
        renameDirectory nextDir tempNextNext
        renameDirectory tempNext nextDir
    let item = joinPath [path, "item"]
    itemExists <- doesFileExist item
    when itemExists $ do
        let nextItem = joinPath [path, "next", "item"]
        makeDir nextDir
        renameFile item nextItem
    writeFile item contents
    