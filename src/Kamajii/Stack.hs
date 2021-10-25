module Kamajii.Stack (processStackCommand) where

import Control.Monad (when)
import Kamajii.Meta (programUsage, headLimit, headLimitExceeded)
import System.EasyFile
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      doesFileExist,
      removeDirectory,
      removeFile,
      renameDirectory,
      renameFile,
      getAppUserDataDirectory,
      joinPath )
import Text.Read (readMaybe)
import Data.List (intercalate)
import System.Posix.Internals (newFilePath)

type Commands = [String]

type StackDir = String

processStackCommand :: Commands -> IO (Maybe String)
processStackCommand (stack : cmd) = getStackDir stack >>= stackAction cmd
processStackCommand _ = return $ Just programUsage

stackAction :: Commands -> StackDir -> IO (Maybe String)
stackAction ("push" : contents) stackDir = pushItem stackDir (unwords contents) >> return Nothing
stackAction ["pop"] stackDir = popItem stackDir
stackAction ["peek"] stackDir = peekItem stackDir
stackAction ["head", rawN] stackDir = case parsedN of
  Just n -> listItems stackDir n
  Nothing -> return Nothing
  where parsedN = readMaybe rawN :: Maybe Int
stackAction ["list"] stackDir = listAllItems stackDir
-- TODO: Read actions.      list, tail N, length, isempty
-- TODO: Lifecycle actions. complete[-all] delete[-all]
-- TODO: Shuffle actions.   swap, rot, next (move first to last)
stackAction _ _ = return (Just programUsage)

-- TODO: These file manipulations could use more abstraction

pushItem :: StackDir -> String -> IO ()
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

peekItem :: StackDir -> IO (Maybe String)
peekItem path = do
  let item = itemOf path
  itemExists <- doesFileExist item
  if itemExists
    then
      ( do
          contents <- readFile item
          return $ Just contents
      )
    else return Nothing

popItem :: StackDir -> IO (Maybe String)
popItem path = do
  contents <- peekItem path
  let item = itemOf path
  whenFileExists item $ do
    removeFile item
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

listAllItems :: StackDir -> IO (Maybe String)
listAllItems path = go path []
  where
    go :: FilePath -> [String] -> IO (Maybe String)
    go path acc = do
      contents <- peekItem path
      case contents of
        Just s -> go (nextOf path) (acc ++ [s])
        Nothing -> present acc
    present :: [String] -> IO (Maybe String)
    present = return . Just . intercalate "\n"

listItems :: StackDir -> Int -> IO (Maybe String)
listItems path 0 = return Nothing
listItems path n
  | n < 0 = return Nothing
  | n > headLimit = return $ Just headLimitExceeded
  | otherwise = go path n []
  where
    go :: FilePath -> Int -> [String] -> IO (Maybe String)
    go _ 0 acc = present acc
    go path n acc = do
      contents <- peekItem path
      case contents of
        Just s -> go (nextOf path) (n-1) (acc ++ [s])
        Nothing -> present acc
    present :: [String] -> IO (Maybe String)
    present = return . Just . intercalate "\n"

-- Stacky stuff

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

pathAppend :: FilePath -> String -> FilePath
pathAppend b a = joinPath [a, b]
