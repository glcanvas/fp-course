
module Block1 where

import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe(fromMaybe)
import Data.Void
import Text.Megaparsec
import Data.Char(ord)
import System.Environment(getArgs)

import Util
import UtilInnerCommands
import DefineDataTypes
--import UtilParserBase
import UtilAssignValues

import Control.Applicative.Combinators
data MachineEnvironment = MachineEnvironment
  {
    declaredValues :: Map.Map String String
    , lastArguments :: String
    , currentDirectory :: FilePath
    , lastCommand :: String
  } deriving Show


innerMachine :: [Statement] -> ReaderT MachineEnvironment IO (MachineEnvironment, ())
innerMachine (AssignRaw ptr value : xs) = do
  currentState <- ask
  let resolver = resolveAssignValue (declaredValues currentState) value
  let updMap = Map.insert ptr resolver (declaredValues currentState)
  local (const currentState{declaredValues = updMap}) (innerMachine xs)

innerMachine (Assign ptr value : xs) = do
  currentState <- ask
  let updMap = Map.insert ptr value (declaredValues currentState)
  local (const currentState{declaredValues = updMap}) (innerMachine xs)

innerMachine [] = do
  v <- ask
  return (v, ())

readFileParseToStatement :: FilePath -> IO (Either (ParseErrorBundle String Void) [Statement])
readFileParseToStatement path = do
  content <- readFile path
  let content' = content <> "\n"
  return $ runParser parserFile "" content'
  where
    printOrd :: String -> IO ()
    printOrd (x:xs) = do
      print (show (ord x) <> " " <> [x])
      printOrd xs
    printOrd [] = pure ()

addValuesToMap :: [String] -> Map.Map String String
addValuesToMap array = wrappedCall array 1
  where
    wrappedCall :: [String] -> Int -> Map.Map String String
    wrappedCall (x:xs) pos =
      let inner = wrappedCall xs (pos + 1) in
      Map.insert (show pos) x inner
    wrappedCall [] _ = Map.empty

block1Execute :: [String] -> IO ()
block1Execute env
  | null env = error "ti chto ohuel?!"
  | otherwise =
    let scriptPath = head env in
      do
        content <- readFileParseToStatement scriptPath
        case content of
          Left _ -> error "ti chto ohuel?!"
          Right statements ->
            let arguments = Map.insert "0" scriptPath (addValuesToMap $ tail env) in
            let call = runReaderT (innerMachine statements)
                  MachineEnvironment {declaredValues = arguments, lastArguments = "", currentDirectory = "" , lastCommand = ""} in
                    do
                      (state, _) <- call
                      print state

               --print statements
call = block1Execute ["/home/nikita/IdeaProjects/fp-homework-templates/hw3/blia.sh"]

type Parser = Parsec Void String

lft :: Parser Char
lft = satisfy (== 'a') *> satisfy (== 'b')

rgh :: Parser Char
rgh =  satisfy (== 'a') *> satisfy (== 'c')

both :: Parser Char
both = try lft <|> try rgh



open :: IO ()
open = do
  v <- readFile "~/IdeaProjects/fp-homework-templates/hw3/README.md"
  putStrLn v