module Main where

import Kamajii.TcpServer (serverMain)

main :: IO ()
main = serverMain "localhost" "3000"
