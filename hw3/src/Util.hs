{-# LANGUAGE InstanceSigs #-}

module Util (
  Expression(..)
  , Statement(..)
  , Parser
  , skip
  , lexeme
  , symbol
  , integer
  , singleQuote
  , identifier
  , parserAssign
  , parserAssignValue
) where

import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Expression = Var String
  | IntConst Integer
  | FloatConst Float
  | SingleQuote String
  | Pointer String
  deriving Show

data Statement = Seq [Statement]
  | Assign String Expression
  | InnerCommand String [String]
  deriving Show

type Parser = Parsec Void String


-- | Skip all whitespace
skip :: Parser ()
skip = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | Wrapper for available skip whitespace after parsed value
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip

-- | Parse satisfy subsequence and skip all whitespace after that
symbol :: String -> Parser String
symbol = L.symbol skip

-- | Parse one integer number
integer :: Parser Integer
integer = lexeme L.decimal

-- | Return all between single quote as string
-- and skip all whitespace after that
singleQuote :: Parser String
singleQuote = {-lexeme $ -}between (symbol "'") (symbol "'") (many (satisfy (/= '\'')))

-- | Parse identifier of equation
-- such as "aaa"
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
  Assign variable <$> parserAssignValue

-- | Wrap parsed string in one of "Expression"
parserAssignValue :: Parser Expression
parserAssignValue = (SingleQuote <$> singleQuote)
  <|> (IntConst <$> L.decimal)
  <|> (Pointer <$> (symbol "$" *> identifier))

gggg = runParser parserAssignValue "hmm" "$adsa"