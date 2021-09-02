module Main where

import Files

main :: IO ()
main = do
  makeAppDir
  home <- appDir
  let next = joinPath [home, "next"]
  makeDir next
  putStrLn $ "home:  " ++ home
  putStrLn $ "next: " ++ next
