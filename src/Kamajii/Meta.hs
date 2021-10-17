module Kamajii.Meta where

programName :: String
programName = "kamajii 0.1"

programUsage :: String
programUsage =
  "Usage: kamajii STACK push CONTENTS...\n"
    ++ "       kamajii STACK pop"

headLimit :: Int
headLimit = 32

headLimitExceeded :: String
headLimitExceeded = "Head limit exceeded. Max: " ++ show headLimit
