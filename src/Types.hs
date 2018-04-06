module Types where

data HeartVal = Nil
              | HeartFalse
              | HeartTrue
              | HeartNumber   Int
              | HeartString   String
              | HeartSymbol   String
              | HeartList     [HeartVal] HeartVal
              deriving (Eq, Show)
