module OuterCommands where

import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar, string)
import Text.Megaparsec
import Data.Char(isSpace)
import qualified Text.Megaparsec.Char.Lexer as L

import UtilParserBase
import UtilAssignValues(oneOfExpr)
import DefineDataTypes


--parserCommandNoQuote :: Parser
