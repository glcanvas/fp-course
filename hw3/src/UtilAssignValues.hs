module UtilAssignValues (
    parserAssign
  , parserAssignValue
  , oneOfExpr
  , parserPointer
  , parseDoubleQuote
  , resolveAssignValue
) where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)


import UtilParserBase
import DefineDataTypes

-- | Parse such substring as
-- Variable=smth
-- where smth is String in single quote or number or other variable
parserAssign :: Parser Statement
parserAssign = do
  isSpaceWithoutEOL
  variable <- assignIdentifier
  satisfy (== '=')
  AssignRaw variable <$> parserEndOfCommand parserAssignValue

-- | Wrap parsed string in one of "AssignValue"
--
-- >>> $a$b$c"asdaasd"'adadsa'
-- [Pointer a, Pointer b ...]
parserAssignValue :: Parser [AssignValue]
parserAssignValue = some oneOfExpr

-- | Function that parse one of substring that satisfy current parametrs
oneOfExpr :: Parser AssignValue
oneOfExpr = try (SingleQuote <$> singleQuote)
      <|> try (DoubleQuote <$> doubleQuote)
      <|> try (SingleQuote <$> aLotOfSheet) --patternIdentifier
      <|> try (Number <$> L.decimal)
      <|> parserPointer

-- | Parser pointer in bash such as
-- $hehmdem
parserPointer :: Parser AssignValue
parserPointer = Pointer <$> (satisfy (== '$') *> patternIdentifier)

-- | resolve inner of string in double quote
parseDoubleQuote :: Map.Map String String -> String -> Either (ParseErrorBundle String Void) String
parseDoubleQuote valueMap s = do
  v <- runParser innerParser "" s
  return $ resolveAssignValue valueMap v
  where
    innerParser :: Parser [AssignValue]
    innerParser = many $ try parserPointer <|> try (SingleQuote <$> dollar) <|> (SingleQuote <$> notDollar)
    dollar :: Parser String
    dollar = correctParse (== '$')
    notDollar :: Parser String
    notDollar = correctParse (/= '$')

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