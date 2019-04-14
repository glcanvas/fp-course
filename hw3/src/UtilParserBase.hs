module UtilParserBase (
  Parser
  , skip
  , isSpaceWithoutEOL
  , isEOL
  , correctParse
  , singleQuote
  , doubleQuote
  , patternIdentifier
  , assignIdentifier
  , parserEndOfCommand
  , aLotOfSheet
) where

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar, string)
import Data.Void
import Data.Char(isSpace)

-- | Define Parser type that need String and Error is Void
type Parser = Parsec Void String

-- | Skip all whitespace
skip :: Parser ()
skip = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | check that sub string not consist end of lines but consist only spaces
isSpaceWithoutEOL :: Parser String
isSpaceWithoutEOL = many (satisfy (\x -> isSpace x && x /= '\n'))

-- | Check that is end of line
isEOL :: Parser String
isEOL = many (satisfy  (=='\n'))

-- | Function that correct parse not empty string for common template
correctParse :: (Char -> Bool) -> Parser String
correctParse func = (:) <$> (satisfy func) <*> many (satisfy func)

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

-- | Parse identifier for template [a-zA-Z_0-9]+
-- such as "aaa" or
-- "privet123"
-- "a______a"
-- "1______1"
patternIdentifier :: Parser String
patternIdentifier = (:) <$> pattern <*> many pattern
  where
    pattern :: Parser Char
    pattern = (try alphaNumChar) <|> satisfy (== '_')

-- | Values that writes after assign character '=' and there isn't in single or double quotes
aLotOfSheet :: Parser String
aLotOfSheet = (:) <$> innerPattern <*> many innerPattern
  where
  innerPattern :: Parser Char
  innerPattern = (try alphaNumChar)
                  <|> try ((\x-> x!!1) <$> (string "\\$")) -- will print only $
                  <|> try ((\x -> head x) <$> (string "\\\\")) -- will print only \
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
    wrapper s = (try s) <|> (satisfy (== '_'))

-- | call at first some parser and than \n or ;
parserEndOfCommand :: Parser a -> Parser a
parserEndOfCommand prs =
  prs <* (isSpaceWithoutEOL *> eolParser)
  where
    eolParser :: Parser Char
    eolParser = satisfy (\x -> x == '\n' || x == ';')