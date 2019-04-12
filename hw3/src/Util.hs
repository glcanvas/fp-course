{-# LANGUAGE InstanceSigs #-}

module Util (
  AssignValue(..)
  , Statement(..)
  , parserAssign
  , parserAssignValue
  , parserPointer
  , parserFile
  , parserCommands
  , resolveAssignValue
  , parseDoubleQuote
) where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar)
import Data.Char(isSpace)
import UtilParserBase

data Statement = Seq [Statement]
  | AssignRaw String [AssignValue]
  | Assign String String
  | InnerCommand String [String]
  deriving Show

data AssignValue = Number Int
  | SingleQuote String
  | DoubleQuote String
  | Pointer String
  deriving (Show, Eq)

-- | Parse such substring as
-- Variable=smth
-- where smth is String in single quote or number or other variable
parserAssign :: Parser Statement
parserAssign = do
  isSpaceWithoutEOL
  variable <- assignIdentifier
  satisfy (== '=')
  AssignRaw variable <$> parserAssignValue

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
      <|> (SingleQuote <$> patternIdentifier)
      <|> (Number <$> L.decimal)
      <|> parserPointer

-- | Combine all Statements in list
parserCommands :: Parser [Statement]
parserCommands = many (isEOL  *> (parserAssign <* isEOL))

-- | Parser pointer in bash such as
-- $hehmdem
parserPointer :: Parser AssignValue
parserPointer = Pointer <$> (satisfy (== '$') *> patternIdentifier)

-- | Function that replace all occurrences of pointer for string that equal such pointer
resolveAssignValue :: Map.Map String String -> [AssignValue] -> String
resolveAssignValue valueMap = innerCall
  where
    innerCall :: [AssignValue] -> String
    innerCall (Number x:xs) = show x <> innerCall xs
    innerCall (SingleQuote x:xs) = x <> innerCall xs
    innerCall (Pointer x:xs) =
      let resolver = Map.lookup x valueMap
       in fromMaybe mempty resolver <> innerCall xs
    innerCall (DoubleQuote x:xs) = fromRight "" (parseDoubleQuote valueMap x)
    innerCall [] = ""

-- | resolve inner of string in double quote
parseDoubleQuote :: Map.Map String String -> String -> Either (ParseErrorBundle String Void) String
parseDoubleQuote valueMap s = do
  v <- runParser innerParser "" s
  return $ resolveAssignValue valueMap v
  where
    innerParser :: Parser [AssignValue]
    innerParser = many $ parserPointer <|> (SingleQuote <$> dollar) <|> (SingleQuote <$> notDollar)
    dollar :: Parser String
    dollar = correctParse (== '$')
    notDollar :: Parser String
    notDollar = correctParse (/= '$')

-- | Main function for all of this
-- that create parse that take apart all commands of file
parserFile :: Parser [Statement]
parserFile = between isEOL (many (satisfy isSpace) *> eof) parserCommands
