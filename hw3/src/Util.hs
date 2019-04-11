{-# LANGUAGE InstanceSigs #-}

module Util {-(
  AssignValue(..)
  , Statement(..)
  , Parser
  , skip
  , parserEndOfCommand
  , symbol
  , singleQuote
  , identifier
  , parserAssign
  , parserAssignValue
)-} where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Text.Megaparsec.Char (spaceChar, crlf, newline, space1, letterChar, alphaNumChar)
import System.Directory
import Data.Char(isSpace)
import Data.String.Utils

data Statement = Seq [Statement]
  | AssignRaw String [AssignValue]
  | Assign String String
  | InnerCommand String [String]
  deriving Show

data AssignValue = Number Int
  | SingleQuote String
  | DoubleQuote String
  | Pointer String
  deriving (Show, Eq)

type Parser = Parsec Void String


-- | Skip all whitespace
skip :: Parser ()
skip = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

isSpaceWithoutEOL :: Parser String
isSpaceWithoutEOL = many (satisfy (\x -> isSpace x && x /= '\n'))

isEOL :: Parser String
isEOL = many (satisfy  (=='\n'))

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
    pattern = alphaNumChar <|> satisfy (== '_')

-- | Parser identifier for template [_a-zA-Z][a-zA-Z0-9_]+
-- for define assign link
assignIdentifier :: Parser String
assignIdentifier = (:) <$> wrapper letterChar <*> many (wrapper alphaNumChar)
  where
    wrapper :: Parser Char -> Parser Char
    wrapper s = s <|> (satisfy (== '_'))

-- | Parse such substring as
-- Variable=smth
-- where smth is String in single quote or number or other variable
parserAssign :: Parser Statement
parserAssign = do
  isSpaceWithoutEOL
  variable <- assignIdentifier
  satisfy (== '=')
  AssignRaw variable <$> parserAssignValue

-- | Wrap parsed string in one of "AssignValue"
--
-- >>> $a$b$c"asdaasd"'adadsa'
-- [Pointer a, Pointer b ...]
parserAssignValue :: Parser [AssignValue]
parserAssignValue =
  parserEndOfCommand $ some oneOfExpr
  where
    oneOfExpr :: Parser AssignValue
    oneOfExpr =
      (SingleQuote <$> singleQuote)
      <|> (DoubleQuote <$> doubleQuote)
      <|> (SingleQuote <$> patternIdentifier)
      <|> (Number <$> L.decimal)
      <|> parserPointer

parserCommands :: Parser [Statement]
parserCommands = many (isEOL  *> (parserAssign <* isEOL))

-- | Parser pointer in bash such as
-- $hehmdem
parserPointer :: Parser AssignValue
parserPointer = Pointer <$> (satisfy (== '$') *> patternIdentifier)

-- | call at first some parser and than \n or ;
parserEndOfCommand :: Parser a -> Parser a
parserEndOfCommand prs =
  prs <* (isSpaceWithoutEOL *> eolParser)
  where
    eolParser :: Parser Char
    eolParser = satisfy (\x -> x == '\n' || x == ';')

-- | Function that replace all occurrences of pointer for string that equal such pointer
resolveAssignValue :: Map.Map String String -> [AssignValue] -> String
resolveAssignValue valueMap array = undefined
  where
    innerCall :: [AssignValue] -> String
    innerCall (Number x:xs) = show x <> innerCall xs
    innerCall (SingleQuote x:xs) = x <> innerCall xs
    innerCall (Pointer x:xs) =
      let resolver = Map.lookup x valueMap in
        fromMaybe mempty resolver <> innerCall xs
    innerCall (DoubleQuote x:xs) =
      fromRight "" (parseDoubleQuote valueMap x)

-- | resolve inner of string in double quote
parseDoubleQuote :: Map.Map String String -> String -> Either (ParseErrorBundle String Void) String
parseDoubleQuote valueMap s = do
  v <- runParser innerParser "" s
  return $ resolveAssignValue valueMap v
  where
    innerParser :: Parser [AssignValue]
    innerParser = many $ parserAsString <|> (SingleQuote `fmap` many (satisfy (/= '$')))

    parserAsString :: Parser AssignValue
    parserAsString = parserPointer <|> (SingleQuote `fmap` (: []) <$> satisfy (== '$'))

-- | Main function for all of this
-- that create parse that take apart all commands of file
parserFile :: Parser [Statement]
parserFile = between isEOL (many (satisfy isSpace) *> eof) parserCommands

gggg = runParser parserAssignValue "hmm"
qweqwe = runParser parserAssignValue "sdf" "asdsada"
--"$adsa$bloa'aaasd'123412$asdasd'asd'\n"


-- | Function that execute all scripts from directory that end with *.sh
executeDir :: FilePath -> IO()
executeDir dir = do
  files <- getDirectoryContents dir
  iterateIO files
  where
    goIfDir :: FilePath -> IO(Bool)
    goIfDir file = doesDirectoryExist $ dir <> file

    goIfShFile :: FilePath -> IO(Bool)
    goIfShFile file = do
      v <- doesFileExist $ dir <> file
      if v
      then
        if endswith ".sh" file
        then return True
        else return False
      else return False

    iterateIO :: [FilePath] -> IO()
    iterateIO (x:xs) = do
      if startswith "." x
        then iterateIO xs
        else inner(x:xs)
      where
        inner (x:xs) = do
        putStrLn x
        result <- goIfDir x
        if result
        then (executeDir $  dir <> x) *> iterateIO xs
        else (executeFile $ dir <> x) *> iterateIO xs

    iterateIO [] = putStrLn $ "end of dir" <> dir

    executeFile :: FilePath -> IO ()
    executeFile file = do
      content <- readFile $ file
      let content' = content <> "\n"
      let value = runParser parserFile "" content'
      print value
      case value of
        Left _ -> error "errr"
        Right _ -> return ()
-- executeDir "/home/nikita/IdeaProjects/haskell-itmo-2019-hw3/task1"
