module UtilInnerCommands (
   parserRead
   , parserEcho
   , parserPwd
   , parserCd
   , parserExit
   --, singleArgument
   , parserPath
) where

import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar, string)
import Text.Megaparsec
import Data.Char(isSpace)
import qualified Text.Megaparsec.Char.Lexer as L

import UtilParserBase
import UtilAssignValues(oneOfExpr)
import DefineDataTypes

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
    first = Echo <$> many singleArgument

    second :: Parser InnerCommand
    second = needDelim *> (EchoWithout <$> many singleArgument)

    needDelim :: Parser String
    needDelim = isSpaceWithoutEOL *> (string "-n" <* isSpaceWithoutEOL)

    singleArgument :: Parser [AssignValue]
    singleArgument = isSpaceWithoutEOL *> (((:) <$> oneOfExpr <*> many oneOfExpr) <* isSpaceWithoutEOL)


--bk = runParser (many singleArgument) "" "a b c"

-- | Inner command that return current full path
parserPwd :: Parser InnerCommand
parserPwd = parserEndOfCommand $ isSpaceWithoutEOL *> (Pwd <$ string "pwd")

-- | simple parse cd such that satisfy until first whitespace
-- I'm suppose that way may be either full either relative
parserCd :: Parser InnerCommand
parserCd = parserEndOfCommand $ isSpaceWithoutEOL *> (Cd <$> (string "cd" *> parserPath))

parserPath :: Parser [AssignValue]
parserPath = undefined -- isSpaceWithoutEOL *> some (satisfy (not . isSpace))


{-
"./" -- loop back
"../" -- dir
split by "/"
/
.
..
\
\"
./"privet "\ kek
runParser parserPath "" "   ../   / "
""
-}


-- | simple parse exit such satisfy template such as "exit <exit code>"
parserExit :: Parser InnerCommand
parserExit = parserEndOfCommand $ isSpaceWithoutEOL *> (Exit <$> (string "exit" *> parserExitCode))
  where
    parserExitCode :: Parser String
    parserExitCode = isSpaceWithoutEOL *> (show <$> L.decimal)