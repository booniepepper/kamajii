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
main = serve (Host "localhost") "3000" handleClient

handleClient :: (Socket, SockAddr) -> IO ()
handleClient (socket, remoteAddr) = do
  putStrLn $ "TCP connection established from " ++ show remoteAddr
  sendLn socket programName
  clientLoop socket
  putStrLn $ "TCP connection terminated from " ++ show remoteAddr

clientLoop :: Socket -> IO ()
clientLoop socket = do
  maybe_bytes <- recv socket 2048
  with maybe_bytes $ \bytes -> do
    let line = unpack bytes
    putStrLn $ "Received command: " ++ line
    let input = words line
    unless (input `elem` [["exit"], ["q"], ["quit"]]) $ do
      maybe_chars <- handleCmd input
      with maybe_chars $ sendLn socket . pack
      clientLoop socket
  where
    with :: Maybe a -> (a -> IO ()) -> IO ()
    with Nothing _ = return ()
    with (Just a) io = io a

    unless :: Bool -> IO () -> IO ()
    unless True _ = return ()
    unless False io = io

type Commands = [String]
type StackDir = String

handleCmd :: Commands -> IO (Maybe String)
handleCmd (stack:cmd) = getStackDir stack >>= stackAction cmd
handleCmd _ = return (Just programUsage)

stackAction :: Commands -> StackDir -> IO (Maybe String)
stackAction ("push":contents) stackDir = pushItem stackDir (unwords contents) >> return Nothing
stackAction ["pop"] stackDir = popItem stackDir
-- TODO: Read actions.      peek, head <n>, list, tail, length, isempty
-- TODO: Lifecycle actions. complete[-all] delete[-all]
-- TODO: Shuffle actions.   swap, rot, next (move first to last)
stackAction _ _ = return (Just programUsage)

sendLn :: MonadIO m => Socket -> ByteString -> m ()
sendLn socket bs = send socket $ snoc bs '\n'
