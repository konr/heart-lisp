module Core where

import qualified Reader
import Types

-- read
heartRead :: String -> Either HeartError HeartVal
heartRead str = Reader.readStr str

-- eval
eval :: a -> String -> a
eval ast env = ast

-- print
heartPrint :: HeartVal -> String
heartPrint exp = show exp
errorPrint :: HeartError -> String
errorPrint (HeartError e) = "### ERROR ###\n" ++ e ++ "\n"

-- loop
loop = do
    putStr "♥ "
    line <- getLine
    case line of
        "" -> return ()
        str -> do
          res <- return $ Reader.readStr str
          out <- case res of
            Left err -> return $ errorPrint err
            Right val -> return $ heartPrint val
          putStrLn out
          loop

main = do
    loop
