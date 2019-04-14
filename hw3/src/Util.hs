{-# LANGUAGE InstanceSigs #-}

module Util (
    parserFile
  , parserCommands
  , bek
) where

import Text.Megaparsec
import Data.Char(isSpace)
import UtilParserBase
import DefineDataTypes
import UtilAssignValues
import UtilInnerCommands

-- | Combine all Statements in list
parserCommands :: Parser [Statement]
parserCommands = many (isEOL  *> innerCommand <* isEOL)
  where
  innerCommand :: Parser Statement
  innerCommand = try (CustomCommand <$> parserRead)
                 <|> try (CustomCommand <$> parserEcho)
                 <|> try (CustomCommand <$> parserPwd)
                 <|> try (CustomCommand <$> parserCd)
                 <|> parserAssign

-- | Main function for all of this
-- that create parse that take apart all commands of file
parserFile :: Parser [Statement]
parserFile = between isEOL (many (satisfy isSpace) *> eof) parserCommands


bek = runParser parserRead "" --runParser parserEcho ""

qq = runParser parserPwd ""