{-# LANGUAGE InstanceSigs #-}

module Util {-(
  AssignValue(..)
  , Statement(..)
  , Parser
  , skip
  , parserEndOfCommand
  , symbol
  , singleQuote
  , identifier
  , parserAssign
  , parserAssignValue
)-} where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar)

data Statement = Seq [Statement]
  | Assign String AssignValue
  | InnerCommand String [String]
  deriving Show

data AssignValue = Number Int
  | SingleQuote String
  | DoubleQuote String
  | Pointer String
  deriving Show

type Parser = Parsec Void String


-- | Skip all whitespace
skip :: Parser ()
skip = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"


-- | Parse satisfy subsequence and skip all whitespace after that
symbol :: String -> Parser String
symbol = L.symbol skip

-- | Return all between single quote as string
-- and don't skip any whitespace
-- "'bla bla privet'"
singleQuote :: Parser String
singleQuote = between (satisfy (== '\'')) (satisfy (=='\'')) (many (satisfy (/= '\'')))
              {-lexeme $ -}

-- | Return all between double quote as string
-- and don't skip any whitespace
-- "bla bla privet"
doubleQuote :: Parser String
doubleQuote = between (satisfy (== '"')) (satisfy (=='"')) (many (satisfy (/= '"')))

-- | Parse identifier of equation
-- such as "aaa" or
-- "privet123"
identifier :: Parser String
identifier = (:) <$> letterChar <*> many alphaNumChar

-- | Parse such substring as
-- Variable=smth
-- where smth is String in single quote or number or other variable
parserAssign :: Parser Statement
parserAssign = do
  variable <- identifier
  satisfy (== '=')
  undefined--Assign variable <$> parserAssignValue

-- | Wrap parsed string in one of "AssignValue"
--
-- >>> $a$b$c"asdaasd"'adadsa'
-- [Pointer a, Pointer b ...]
parserAssignValue :: Parser [AssignValue]
parserAssignValue =
  parserEndOfCommand $ some oneOfExpr
  where
    oneOfExpr :: Parser AssignValue
    oneOfExpr =
      (SingleQuote <$> singleQuote)
      <|> (DoubleQuote <$> doubleQuote)
      <|> (SingleQuote <$> identifier)
      <|> (Number <$> L.decimal)
      <|> parserPointer

-- | Parser pointer in bash such as
-- $hehmdem
parserPointer :: Parser AssignValue
parserPointer = Pointer <$> (satisfy (== '$') *> identifier)

-- | call at first some parser and than \n or ;
parserEndOfCommand :: Parser a -> Parser a
parserEndOfCommand prs =
  prs <* (whParser *> eolParser)
  where
    whParser :: Parser String
    whParser = many spaceChar

    eolParser :: Parser Char
    eolParser = satisfy (\x -> x == '\n' || x == ';')

resolveAssignValue :: Map.Map String String -> [AssignValue] -> String
resolveAssignValue valueMap array = undefined
  where
    innerCall :: [AssignValue] -> String
    innerCall (Number x:xs) = show x <> innerCall xs
    innerCall (SingleQuote x:xs) = x <> innerCall xs
    innerCall (Pointer x:xs) =
      let resolver = Map.lookup x valueMap in
        fromMaybe mempty resolver <> innerCall xs
    innerCall (DoubleQuote x:xs) =
      fromRight "" (parseDoubleQuote valueMap x)

-- | resolve inner of string in double quote
parseDoubleQuote :: Map.Map String String -> String -> Either (ParseErrorBundle String Void) String
parseDoubleQuote valueMap s = do
  v <- runParser innerParser "" s
  return $ resolveAssignValue valueMap v
  where
    innerParser :: Parser [AssignValue]
    innerParser = many $ parserAsString <|> (SingleQuote `fmap` many (satisfy (/= '$')))

    parserAsString :: Parser AssignValue
    parserAsString = parserPointer <|> (SingleQuote `fmap` (: []) <$> satisfy (== '$'))
-- \n -- eol
parserFile :: Parser a -> Parser a
parserFile = between skip eof

gggg = runParser parserAssignValue "hmm"
--"$adsa$bloa'aaasd'123412$asdasd'asd'\n"