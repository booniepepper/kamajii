module Main where

import Files

main :: IO ()
main = do
  putStrLn "Enter a stack name:"
  stackName <- getLine
  stackDir <- getStackDir stackName
  putStrLn stackDir
