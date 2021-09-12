module Main where

import Files

main :: IO ()
main = do
  putStrLn "Enter a stack name:"
  stackName <- getLine
  stackDir <- getStackDir stackName
  putStrLn "Enter some content:"
  content <- getLine
  writeFile (stackDir ++ "/item") (content ++ "\n")
  written <- readFile (stackDir ++ "/item")
  putStrLn $ "Wrote the following: [" ++ written ++ "]"
