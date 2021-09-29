module Main where

import Control.Monad
import Data.Char
import Files

main :: IO ()
main = do
  stackName <- promptLn "Enter a stack name:"
  stackDir <- getStackDir stackName

  action <- promptLn "Push or pop?"

  when (map toLower action == "push") $ do
    putStrLn "Enter some content:"
    contents <- getLine
    pushItem stackDir contents

  when (map toLower action == "pop") $ do
    contents <- popItem stackDir
    forM_ contents putStrLn

prompt :: String -> IO String
prompt s = putStr s >> getLine

promptLn :: String -> IO String
promptLn s = putStrLn s >> getLine
