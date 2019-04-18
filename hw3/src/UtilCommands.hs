module UtilCommands where

{- parserRead
   , parserEcho
   , parserPwd
   , parserCd
   , parserExit
   --, singleArgument
   , parserPath
   , parserOneInnerCommand
-}

import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar, string)
import Text.Megaparsec
import Data.Char(isSpace)
import qualified Text.Megaparsec.Char.Lexer as L

import UtilParserBase
import DefineDataTypes

-- | Function that parse one of substring that satisfy current parametrs
assignedExpression :: Parser AssignValue
assignedExpression = try (SingleQuote <$> singleQuote)
      <|> try (DoubleQuote <$> doubleQuote)
      <|> try (SingleQuote <$> aLotOfSheet)
      <|> try (AssignCommand <$> parserCommandInThread)
      <|> parserPointer

-- | Return all between double quote as "AssignValue"
-- and don't skip any whitespace
-- in inner code VERY IMPORTANT ORDER OF functions parserPoint, parserCommand, parserOther
doubleQuote :: Parser [AssignValue]
doubleQuote = between (single '"') (single '"') wrap
  where
    wrap :: Parser [AssignValue]
    wrap = many wrapOne

    wrapOne :: Parser AssignValue
    wrapOne = try borderCases <|> try parserCommand <|> try parserPoint <|> try parserOther

    borderCases :: Parser AssignValue
    borderCases = (\x -> SingleQuote [x]) <$> (try ((!! 1) <$> string "\\\"") <|> (head <$> string "\\\\"))

    parserPoint :: Parser AssignValue
    parserPoint = Pointer <$> (string "$" *> patternIdentifier)

    parserCommand :: Parser AssignValue
    parserCommand = AssignCommand <$> parserCommandInThread

    parserOther :: Parser AssignValue
    parserOther = SingleQuote <$> ((:) <$> satisfy (/= '"') <*> many (satisfy (\x -> x /= '"' && x /= '$')))

-- This command may be write only in this $() construction
-- function satisfy condition that inner of "$(...)" may be empty
parserCommandInThread :: Parser [ShellCommands]
parserCommandInThread = between (string "$(") (string ")") (try wrapper <|> [] <$ isSpaceWithoutEOL)
  where
  -- | here function that wrap into yourself innerParserCommand or pass whitespace
  wrapper :: Parser [ShellCommands]
  wrapper = many combineCommandWithCMD

  -- | function that combine one command that may ended by nothing or by command delimetr
  combineCommandWithCMD :: Parser ShellCommands
  combineCommandWithCMD = try (parserEndOfCommand passWhitespace) <|> passWhitespace

  -- | parser one command that splited by any whitespace without \n
  passWhitespace :: Parser ShellCommands
  passWhitespace = between isSpaceWithoutEOL isSpaceWithoutEOL innerParserCommand

  -- | here function that parse only one command that may be either inner or external
  innerParserCommand :: Parser ShellCommands
  innerParserCommand = try parserOneInnerCommand <|> parserExternalCommand


-- | Function that parse one external command
parserExternalCommand :: Parser ShellCommands
parserExternalCommand =
    ExternalCommandConst <$>
    (ExternalConst <$> between isSpaceWithoutEOL isSpaceWithoutEOL assignIdentifier <*>
     many (between isSpaceWithoutEOL isSpaceWithoutEOL assignedExpression))

---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------- INNER COMMANDS -------------------------------------------------------

-- | Inner command that read arguments from console
parserRead :: Parser ShellCommands
parserRead = InnerCommandConst <$> (isSpaceWithoutEOL *> innerParser)
  where
    innerParser :: Parser InnerCommand
    innerParser = Read <$> (string "read" *> try (many singleArgument))
    singleArgument :: Parser String
    singleArgument = isSpaceWithoutEOL *> (patternIdentifier <* isSpaceWithoutEOL)

-- | Inner command that write arguments to console
parserEcho :: Parser ShellCommands
parserEcho = InnerCommandConst <$> (isSpaceWithoutEOL *> innerParser)
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
    singleArgument = isSpaceWithoutEOL *> (((:) <$> assignedExpression <*> many assignedExpression) <* isSpaceWithoutEOL)

--bk = runParser (many singleArgument) "" "a b c"

-- | Inner command that return current full path
parserPwd :: Parser ShellCommands
parserPwd = InnerCommandConst <$> (isSpaceWithoutEOL *> (Pwd <$ string "pwd"))

-- | simple parse cd such that satisfy until first whitespace
-- I'm suppose that way may be either full either relative
parserCd :: Parser ShellCommands
parserCd = InnerCommandConst <$> (isSpaceWithoutEOL *> (Cd <$> (string "cd" *> parserPath)))

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
parserExit :: Parser ShellCommands
parserExit = InnerCommandConst <$> (isSpaceWithoutEOL *> (Exit <$> (string "exit" *> parserExitCode)))
  where
    parserExitCode :: Parser String
    parserExitCode = isSpaceWithoutEOL *> (show <$> L.decimal)

-- | function that satisfy one of inner command from block 3
parserOneInnerCommand :: Parser ShellCommands
parserOneInnerCommand = try parserRead
                        <|> try parserEcho
                        <|> try parserPwd
                        <|> try parserCd
                        <|> try parserExit