module Main where

import Files

main :: IO ()
main = do
  makeAppDir
  home <- appDir
  putStrLn $ "home: " ++ home
