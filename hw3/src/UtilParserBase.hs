module UtilParserBase (
  Parser
  , isSpaceWithoutEOL
  , oneSpaceWithoutEOL
  , isEOL
  , singleSpace
  , correctParse
  , singleQuote
  , patternIdentifier
  , assignIdentifier
  , parserEndOfCommand
  , aLotOfSheet
  , parserPointer
  , containsKey
  , passWhitespace
) where

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar, string)
import Data.Void
import Data.Char(isSpace)
import Data.List

import DefineDataTypes

-- | Define Parser type that need String and Error is Void
type Parser = Parsec Void String

-- | single space that not equal \n
singleSpace :: Parser Char
singleSpace = satisfy (\x -> isSpace x && x /= '\n')

-- | skip all whitespaces while can
isSpaceWithoutEOL :: Parser ()
isSpaceWithoutEOL = skipMany singleSpace

-- | skip at least ONE whitespaces while can
oneSpaceWithoutEOL :: Parser ()
oneSpaceWithoutEOL = skipSome singleSpace

-- | skip all end of lines as can
isEOL :: Parser ()
isEOL = skipMany (satisfy  (=='\n'))

-- | Function that correct parse not empty string for common template
correctParse :: (Char -> Bool) -> Parser String
correctParse func = (:) <$> (satisfy func) <*> many (satisfy func)

-- | Return all between single quote as string
-- and don't skip any whitespace
-- "'bla bla privet'"
singleQuote :: Parser String
singleQuote = between (single '\'') (single '\'') (many (satisfy (/= '\'')))

-- | Parser pointer in bash such as
-- $hehmdem
parserPointer :: Parser AssignValue
parserPointer = Pointer <$> (single '$' *> patternIdentifier)

-- | Parse identifier for template [a-zA-Z_0-9]+
-- such as "aaa" or
-- "privet123"
-- "a______a"
-- "1______1"
-- "___"
patternIdentifier :: Parser String
patternIdentifier = (:) <$> pattern <*> many pattern
  where
    pattern :: Parser Char
    pattern = (try alphaNumChar) <|> single '_'

-- | Values that writes after assign character '=' and there isn't in single or double quotes
-- must be at end of alternative
aLotOfSheet :: Parser String
aLotOfSheet = (:) <$> innerPattern <*> many innerPattern
  where
  innerPattern :: Parser Char
  innerPattern = (try alphaNumChar)
                  <|> try ((!!1) <$> string "\\$") -- will print only $
                  <|> try (head <$> string "\\\\") -- will print only \
                  <|> try ((!!1) <$> string "\\\"") -- will print only "
                  <|> satisfy (\x ->
                            x /= ';'
                         && x /= '$'
                         && x /= '\''
                         && x /= '"'
                         && x /= '`'
                         && x /= '|'
                         && x /= '('
                         && x /=')'
                         && x /= '\\'
                         && x /= '>'
                         && x /= '<'
                         && x /= '{'
                         && x /= '}'
                         && not (isSpace x)) -- can't be without quotes

-- | Parser identifier for template [_a-zA-Z][a-zA-Z0-9_]+
-- for define assign link
assignIdentifier :: Parser String
assignIdentifier = (:) <$> wrapper letterChar <*> many (wrapper alphaNumChar)
  where
    wrapper :: Parser Char -> Parser Char
    wrapper s = (try s) <|> single '_'

-- | call at start of file and some parser and than \n or ;
-- need to insert after commands
parserEndOfCommand :: Parser a -> Parser a
parserEndOfCommand prs =
  prs <* (isSpaceWithoutEOL *> endOfCommandParser)
  where
    endOfCommandParser :: Parser Char
    endOfCommandParser = satisfy (\x -> x == '\n' || x == ';')

-- | function that return true and all after key if second argument contains
-- in first and before this exist only whitespace
-- and after founded pattern is end or spaces
-- first argument is value
-- second arguments is key that will be founded
containsKey :: String -> String -> (Bool, String)
containsKey value pattern =
  if value == pattern
    then (True, mempty)
    else inner value

  where
    inner :: String -> (Bool, String)
    inner (x:xs) =
      if isSpace x
        then inner xs
        else
          if isPrefixOf pattern (x:xs)
          then
            let valueTail = drop (length pattern) (x:xs) in
              if null valueTail || isSpace (head valueTail)
                then (True, valueTail)
                else (False, mempty)
          else (False, mempty)
    inner [] = (False, mempty)

-- | pass simple whitespaces
passWhitespace :: Parser a -> Parser a
passWhitespace = between isSpaceWithoutEOL isSpaceWithoutEOL

