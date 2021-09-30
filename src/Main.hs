module Main where

import Control.Monad ( forM_ )
import Files ( getStackDir, pushItem, popItem )
import System.IO (isEOF)

programName :: String
programName = "kamajii 0.1"

programUsage :: String
programUsage =
  "Usage: kamajii STACK push CONTENTS..." ++
  "       kamajii STACK pop"

main :: IO ()
main = do
  putStrLn programName
  putStrLn $ take (length programName) (cycle "~")
  loop

loop :: IO ()
loop = do
  end <- isEOF
  if end then
    return ()
  else do
    line <- getLine
    let command = words line
    handle command
    loop

type Commands = [String]
type StackDir = String

handle :: Commands -> IO ()
handle (stack:cmd) = getStackDir stack >>= \dir -> stackAction dir cmd
handle _ = putStrLn programUsage

stackAction :: StackDir -> Commands -> IO ()
stackAction stackDir ("push":contents) = pushItem stackDir (unwords contents)
stackAction stackDir ["pop"] = popItem stackDir >>= \contents -> forM_ contents putStrLn
-- TODO: Read actions.      peek, head <n>, list, tail, length, isempty
-- TODO: Lifecycle actions. complete[-all] delete[-all]
-- TODO: Shuffle actions.   swap, rot, next (move first to last)
stackAction _ _ = putStrLn programUsage
