module Files where

import System.EasyFile
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getAppUserDataDirectory,
    joinPath,
  )

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
