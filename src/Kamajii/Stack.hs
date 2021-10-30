module Kamajii.Stack (processStackCommand) where

import Control.Monad (when)
import Control.Seq (rdeepseq, seqList, using)
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
import Data.Char (toUpper)
import Data.List (intercalate)
import System.Posix.Internals (newFilePath)

type Commands = [String]

type StackDir = String

parseError :: IO (Maybe String)
parseError = return . Just $ "ERROR"

processStackCommand :: Commands -> IO (Maybe String)
processStackCommand ["help"] = return (Just programUsage)
processStackCommand (stack : cmd) = getStackDir stack >>= stackAction cmd
processStackCommand _ = parseError

parsingInt :: String -> (Int -> IO (Maybe String)) -> IO (Maybe String)
parsingInt rawN f = maybe parseError f (readMaybe rawN)

stackAction :: Commands -> StackDir -> IO (Maybe String)
stackAction ("push" : contents) stackDir = pushItem stackDir (unwords contents) >> return Nothing
stackAction ["pop"] stackDir = popItem stackDir
stackAction ["peek"] stackDir = peekItem stackDir
stackAction ["head", n] stackDir = parsingInt n $ listItems stackDir
stackAction ["list"] stackDir = listAllItems stackDir
stackAction ["tail", n] stackDir = return (Just "TODO: NOT YET IMPLEMENTED")
stackAction ["length"] stackDir = fmap (Just . show) (getLength stackDir)
stackAction ["empty"] stackDir = fmap (Just . map toUpper . show . (== 0)) (getLength stackDir)
-- TODO: Lifecycle actions. complete[-all] delete[-all] ... Or is this a Sigi idea only?
-- TODO: More stack actions?
stackAction ["swap"] stackDir = return (Just "TODO: NOT YET IMPLEMENTED")
stackAction ["rot"] stackDir = return (Just "TODO: NOT YET IMPLEMENTED")
stackAction ["next"] stackDir = return (Just "TODO: NOT YET IMPLEMENTED")
stackAction _ _ = parseError

-- TODO: These file manipulations could use more abstraction

getLength :: StackDir -> IO Int
getLength path = do
  let countFile = countOf path
  fileExists <- doesFileExist countFile
  if fileExists
    then fmap read (readFile countFile)
    else return 0

updateLength :: StackDir -> (Int -> Int) -> IO ()
updateLength path f = do
  let countFile = countOf path
  count <- getLength path
  let strictCount = count `using` rdeepseq -- force evaluation. TODO: This fails
  let newCount = maximum [0, f strictCount] -- never go below 0 length
  writeFile countFile $ show newCount

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
  updateLength path succ

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
  updateLength path pred
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

countOf :: FilePath -> FilePath
countOf = pathAppend "count"

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
