module Core where

import qualified Reader
import Types

-- aux
rep :: String -> IO String
rep line = heartRead line >>= \ast -> return $ heartPrint (eval ast mempty)

-- read
heartRead :: String -> IO HeartVal
heartRead str = Reader.readStr str

-- eval
eval :: a -> String -> a
eval ast env = ast

-- print
heartPrint :: HeartVal -> String
heartPrint exp = show exp

-- loop
loop = do
    putStr "♥ "
    line <- getLine
    case line of
        "" -> return ()
        str -> do
          x <- rep str
          putStrLn $ x
          loop

main = do
    loop
