module Main where

import System.EasyFile (getAppUserDataDirectory, createDirectoryIfMissing)

appDir = getAppUserDataDirectory "kamajii"
makeDir = createDirectoryIfMissing True

main :: IO ()
main = do
  home <- appDir
  putStrLn ("home: " ++ home)
  makeDir home

