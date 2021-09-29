module Files (getAppDir, getCustomStackDir, getStackDir, pushItem, popItem) where

import Control.Monad
import System.EasyFile
import System.IO

-- File/Directory manipulation

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

-- Stacky stuff

pathAppend :: FilePath -> String -> FilePath
pathAppend b a = joinPath [a, b]

itemOf :: FilePath -> FilePath
itemOf = pathAppend "item"

nextOf :: FilePath -> FilePath
nextOf = pathAppend "next"

tempOf :: FilePath -> FilePath
tempOf = pathAppend "temp"

nextItemOf :: FilePath -> FilePath
nextItemOf = itemOf . nextOf

nextNextOf :: FilePath -> FilePath
nextNextOf = nextOf . nextOf

whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = do
    result <- test
    when result action

whenDirExists :: FilePath -> IO () -> IO ()
whenDirExists path = whenM (doesDirectoryExist path)

whenFileExists :: FilePath -> IO () -> IO ()
whenFileExists path = whenM (doesFileExist path)

pushItem :: FilePath -> String -> IO ()
pushItem path contents = do
    let nextDir = nextOf path
    whenDirExists nextDir $ do
        let tempNext = tempOf path
        let tempNextNext = nextOf . tempOf $ path
        makeDir tempNext
        renameDirectory nextDir tempNextNext
        renameDirectory tempNext nextDir
    let item = itemOf path
    whenFileExists item $ do
        let nextItem = nextItemOf path
        makeDir nextDir
        renameFile item nextItem
    writeFile item contents

popItem :: FilePath -> IO (Maybe String)
popItem path = do
    let item = itemOf path
    itemExists <- doesFileExist item
    contents <- if itemExists then (do
                    contents <- readFile item
                    removeFile item
                    return (Just contents)) else return Nothing
    let nextItem = nextItemOf path
    whenFileExists nextItem $ do
        renameFile nextItem item
    let nextDir = nextOf path
    let nextNextDir = nextNextOf path
    whenDirExists nextNextDir $ do
        let tempNextDir = tempOf path
        renameDirectory nextNextDir tempNextDir
        removeDirectory nextDir
        renameDirectory tempNextDir nextDir
    nextDirExists <- doesDirectoryExist nextDir
    nextItemStillExists <- doesFileExist nextItem
    when (nextDirExists && not nextItemStillExists) $ do
        removeDirectory nextDir
    return contents
