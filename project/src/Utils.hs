{-# LANGUAGE Strict#-}

module Utils (
  readProperties
) where

import Text.Megaparsec (Parsec, between, skipMany, satisfy, eof, try, single, some, many, runParser, (<|>))
import Text.Megaparsec.Char (alphaNumChar)
import Data.Void(Void)
import Data.Char(isSpace)
import GHC.IO(catchAny)
import qualified Data.Map as Map(Map, fromList, empty)

type Parser = Parsec Void String

spaces :: Parser ()
spaces = skipMany $ satisfy isSpace


singleChar :: Parser Char
singleChar =
  try alphaNumChar <|> try (single '.') <|> single ':'

singleWord :: Parser String
singleWord = some singleChar

singleKeyValue :: Parser (String, String)
singleKeyValue = ((,) <$> singleWord) <*> (single '=' *> singleWord)

wrapKeyValue :: Parser (String, String)
wrapKeyValue = between spaces spaces singleKeyValue

properties :: Parser (Map.Map String String)
properties = between spaces (spaces *> eof) (Map.fromList <$> many wrapKeyValue)

-- | Read file with properties in format:
--  key=value and write parsed values to Map
readProperties :: String -> IO (Map.Map String String)
readProperties filePath = catchAny
  (do
  content <- readFile filePath
  let kv = runParser properties filePath content
  case kv of
    (Right result) -> pure result
    (Left _) -> pure Map.empty) (\_ -> pure Map.empty)