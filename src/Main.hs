module Main where

import Control.Monad ( forM_ )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, snoc, unpack)
import qualified Data.ByteString as B
import Files ( getStackDir, pushItem, popItem )
import Network.Simple.TCP
    ( recv, send, serve, SockAddr, Socket, HostPreference(Host) )
import System.IO (isEOF)
import Control.Monad.IO.Class ( MonadIO )

programName :: ByteString
programName = pack "kamajii 0.1"

programUsage :: String
programUsage =
  "Usage: kamajii STACK push CONTENTS...\n" ++
  "       kamajii STACK pop"

main :: IO ()
main = serve (Host "localhost") "3000" mainSession

mainSession :: (Socket, SockAddr) -> IO ()
mainSession (socket, remoteAddr) = do
  putStrLn $ "TCP connection established from " ++ show remoteAddr
  sendLn socket programName
  loop socket
  putStrLn $ "TCP connection terminated from " ++ show remoteAddr

loop :: Socket -> IO ()
loop socket = do
  maybe_input <- recv socket 2048
  case maybe_input of
    Nothing -> return ()
    Just bytes -> do
      let input = words . unpack $ bytes
      if input `elem` [["exit"], ["q"], ["quit"]]
      then return ()
      else do
        maybe_output <- handle input
        case maybe_output of
          Nothing -> return ()
          Just chars -> sendLn socket (pack chars)
        loop socket

type Commands = [String]
type StackDir = String

handle :: Commands -> IO (Maybe String)
handle (stack:cmd) = getStackDir stack >>= \dir -> stackAction dir cmd
handle _ = return (Just programUsage)

stackAction :: StackDir -> Commands -> IO (Maybe String)
stackAction stackDir ("push":contents) = pushItem stackDir (unwords contents) >> return Nothing
stackAction stackDir ["pop"] = popItem stackDir
-- TODO: Read actions.      peek, head <n>, list, tail, length, isempty
-- TODO: Lifecycle actions. complete[-all] delete[-all]
-- TODO: Shuffle actions.   swap, rot, next (move first to last)
stackAction _ _ = return (Just programUsage)

sendLn :: MonadIO m => Socket -> ByteString -> m ()
sendLn socket bs = send socket $ snoc bs '\n'
