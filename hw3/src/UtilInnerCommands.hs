module UtilInnerCommands (
   parserRead
   , parserEcho
   , parserPwd
   , parserCd
   --, second
   --, singleArgument
) where

import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar, string)
import Text.Megaparsec
import Data.Char(isSpace)

import UtilParserBase
import UtilAssignValues(oneOfExpr)
import DefineDataTypes
--import Control.Applicative


-- | Inner command that read arguments from console
parserRead :: Parser InnerCommand
parserRead = parserEndOfCommand $ isSpaceWithoutEOL *> innerParser
  where
  innerParser :: Parser InnerCommand
  innerParser = Read <$> (string "read" *> try (many singleArgument))

  singleArgument :: Parser String
  singleArgument = isSpaceWithoutEOL *> (patternIdentifier <* isSpaceWithoutEOL)

-- | Inner command that write arguments to console
parserEcho :: Parser InnerCommand
parserEcho = parserEndOfCommand $ isSpaceWithoutEOL *> innerParser
  where
    innerParser :: Parser InnerCommand
    innerParser = (string "echo" *> isSpaceWithoutEOL) *> (try second <|> try first )

    first :: Parser InnerCommand
    first = EchoWithout <$> many singleArgument

    second :: Parser InnerCommand
    second = needDelim *> (Echo <$> many singleArgument)

    needDelim :: Parser String
    needDelim = isSpaceWithoutEOL *> (string "-n" <* isSpaceWithoutEOL)

    singleArgument :: Parser [AssignValue]
    singleArgument = isSpaceWithoutEOL *> (((:) <$> oneOfExpr <*> many oneOfExpr) <* isSpaceWithoutEOL)


--bk = runParser (many singleArgument) "" "a b c"

-- | Inner command that return current full path
parserPwd :: Parser InnerCommand
parserPwd = parserEndOfCommand $ isSpaceWithoutEOL *> (Pwd <$ string "pwd")

-- | simple parse cd such that satisfy until first whitespace
parserCd :: Parser InnerCommand
parserCd = parserEndOfCommand $ isSpaceWithoutEOL *> (Cd <$> (string "cd" *> parserPath))
  where
    parserPath :: Parser FilePath
    parserPath = isSpaceWithoutEOL *> some (satisfy (not . isSpace))
