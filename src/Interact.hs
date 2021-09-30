module Interact () where

-- Unused experiments on some ideas of interaction

class Interactive f where
    interact :: f -> IO ()

instance Interactive (String -> String) where
    interact = Prelude.interact

instance Interactive (String -> IO ()) where
    interact f = do s <- getContents
                    f s

instance Interactive (String -> IO String) where
    interact f = do s <- getContents
                    r <- f s
                    putStr r

instance Interactive (String -> Maybe String) where
    interact f = do s <- getContents
                    mapM_ putStr (f s)

instance Interactive (String -> IO (Maybe String)) where
    interact f = do s <- getContents
                    r <- f s
                    mapM_ putStr r

prompt :: String -> IO String
prompt s = putStr s >> getLine

promptLn :: String -> IO String
promptLn s = putStrLn s >> getLine
 
-- TODO: These are not prompting each line
-- class Interactive f => Prompt s f where
--     prompt :: s -> f -> IO ()

-- instance Prompt String f where
--     prompt s f = putStr s >> interact f

-- instance Prompt (IO String) f where
--     prompt s f = s >>= putStr >> interact f

-- instance Prompt (Maybe String) f where
--     prompt s f = mapM_ putStr s >> interact f
