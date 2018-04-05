module Core where

-- aux
rep :: a -> a
rep line = lispPrint $ eval (lispRead line) ""

-- read
lispRead :: a -> a
lispRead str = str

-- eval
eval :: a -> b -> a
eval ast env = ast

-- print
lispPrint :: a -> a
lispPrint exp = exp

-- loop
loop = do
    putStr "♥ "
    line <- getLine
    case line of
        "" -> return ()
        str -> do
          putStrLn $ rep str
          loop

main = do
    loop
