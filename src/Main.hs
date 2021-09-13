module Main where

import Files

main :: IO ()
main = do
  putStrLn "Enter a stack name:"
  stackName <- getLine
  stackDir <- getStackDir stackName
  putStrLn "Enter some content:"
  content <- getLine
  pushItem stackDir content
