module Main where

import Files

main :: IO ()
main = do
  home <- appDir
  putStrLn $ "home: " ++ home
  makeDir home
