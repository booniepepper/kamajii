module Main where

import Files

main :: IO ()
main =
  appDir >>= \home ->
  putStrLn ("home: " ++ home) >>
  makeDir home

