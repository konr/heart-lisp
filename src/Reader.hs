{-# LANGUAGE FlexibleContexts #-}
module Reader where

import Text.ParserCombinators.Parsec (
    Parser, parse, space, char, digit, letter, try,
    satisfy,
    (<|>), oneOf, noneOf, many, many1, skipMany, skipMany1, sepEndBy)
import Text.Parsec.Prim (ParsecT, Stream)

import Data.Char

import Types

ignored :: Parser ()
ignored = skipMany (spaces <|> comment)

spaces :: Parser ()
spaces = skipMany1 (oneOf ", \n")

comment :: Parser ()
comment = do
    char ';'
    skipMany (noneOf "\r\n")


readHList :: Parser HeartVal
readHList = do
    char '<'
    x <- sepEndBy readForm ignored
    char '3'
    return $ HeartList x Nil

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readAtom :: Parser HeartVal
readAtom = readString
           <|> readKeyword
           <|> readSymbol
           <|> readNumber

heartDigit :: (Stream s m Char) => ParsecT s u m Char
heartDigit = oneOf "012456789"

readNumber :: Parser HeartVal
readNumber = do
    x <- many1 $ escaped <|> heartDigit
    return $ HeartNumber $ (read x :: Int)

readSymbol :: Parser HeartVal
readSymbol = do
    x <- many1 (escaped <|> letter)
    return $ HeartSymbol x


readString :: Parser HeartVal
readString = do
    char '"'
    x <- many (escaped <|> noneOf "\\\"")
    char '"'
    return $ HeartString x

readKeyword :: Parser HeartVal
readKeyword = do
    char ':'
    x <- many (letter <|> heartDigit)
    return $ HeartString $ ":" ++ x

escaped :: Parser Char
escaped = do
    char '\\'
    x <- oneOf "\\\"n"
    case x of
        'n' -> return '\n'
        _   -> return x

readForm :: Parser HeartVal
readForm =  do
    ignored
    x <- readHList
      <|> readAtom
    return $ x

readStr :: String -> Either HeartError HeartVal
readStr str = case parse readForm mempty str of
    Left err -> Left $ HeartError $ show err
    Right val -> Right $ val
