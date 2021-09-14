module Main where

import Control.Monad
import Data.Char
import Files

main :: IO ()
main = do
  putStrLn "Enter a stack name:"
  stackName <- getLine
  stackDir <- getStackDir stackName
  putStrLn "Push or pop?"
  action <- getLine
  when (map toLower action == "push") $ do
    putStrLn "Enter some content:"
    contents <- getLine
    pushItem stackDir contents
  when (map toLower action == "pop") $ do
    contents <- popItem stackDir
    case contents of
      Just contents -> putStrLn contents
      Nothing -> return ()
