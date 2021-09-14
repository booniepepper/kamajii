module Files (getAppDir, getCustomStackDir, getStackDir, pushItem, popItem) where

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

popItem :: FilePath -> IO (Maybe String)
popItem path = do
    let item = joinPath [path, "item"]
    itemExists <- doesFileExist item
    contents <- case itemExists of
        True -> do
            contents <- readFile item
            removeFile item
            return (Just contents)
        False -> return Nothing
    let nextItem = joinPath [path, "next", "item"]
    nextItemExists <- doesFileExist nextItem
    when nextItemExists $ do
        renameFile nextItem item
    let nextDir = joinPath [path, "next"]
    let nextNextDir = joinPath [path, "next", "next"]
    nextNextDirExists <- doesDirectoryExist nextNextDir
    when nextNextDirExists $ do
        let tempNextDir = joinPath [path, "next-temp"]
        renameDirectory nextNextDir tempNextDir
        removeDirectory nextDir
        renameDirectory tempNextDir nextDir
    nextItemStillExists <- doesFileExist nextItem
    unless nextItemStillExists $ do
        removeDirectory nextDir
    return contents
