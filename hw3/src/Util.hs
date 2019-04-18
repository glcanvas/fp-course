{-# LANGUAGE InstanceSigs #-}

module Util (
    parserFile
  , parserCommands
) where

import Text.Megaparsec
import Data.Char(isSpace)

import UtilParserBase
import DefineDataTypes
import UtilAssignValues
import UtilCommands

-- | Combine all Statements in list
parserCommands :: Parser [Statement]
parserCommands = many (isEOL *> innerCommand <* isEOL)
  where
    innerCommand :: Parser Statement -- Here if and while must be at first place otherwise it will be parsered and (maybe)fail by  CustomCommand
    innerCommand = try (CustomCommand <$> parserOneInnerCommand) -- parser one of command not in $()
                   <|> try (ThreadCommand <$> parserEndOfCommand parserCommandInThread) -- parser command that is in $()
                   <|> parserAssign -- parser assign value

-- | Main function for all of this
-- that create parse that take apart all commands of file
parserFile :: Parser [Statement]
parserFile = between isEOL (many (satisfy (\x -> isSpace x || x == ';')) *> eof) parserCommands
