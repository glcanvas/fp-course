{-# LANGUAGE TemplateHaskell #-}

module Block1 where

import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe(fromMaybe)
import Data.Void
import Text.Megaparsec
import Data.Char(ord)
import System.Environment(getArgs)
import Control.Lens
import Data.List

import DefineDataTypes
import UtilAssignValues
import UtilInnerCommands
import Util
--import UtilParserBase

import Control.Applicative.Combinators
data MachineEnvironment = MachineEnvironment
  {
    _declaredValues :: Map.Map String String
    , _currentDirectory :: FilePath
  } deriving Show

makeLenses ''MachineEnvironment

innerMachine :: [Statement] -> ReaderT MachineEnvironment IO (MachineEnvironment, ())
innerMachine (AssignRaw ptr value:xs) = do
  currentState <- ask
  let resolver = resolveAssignValue (currentState^.declaredValues) value
  let updMap = Map.insert ptr resolver (currentState^.declaredValues)
  local (declaredValues.~updMap) (innerMachine xs)

innerMachine (CustomCommand cmd:xs) = innerMatch cmd
  where
    innerMatch :: InnerCommand -> ReaderT MachineEnvironment IO (MachineEnvironment, ())
    innerMatch (Read args) = do
      currentState <- ask
      readString <- lift getLine
      let zipped = correctnessZip args (words readString)
      let updMap = foldr (\(k, v) b -> Map.insert k v b) (currentState^.declaredValues) zipped
      local (declaredValues.~updMap) (innerMachine xs)

    innerMatch (Echo args) = undefined
    innerMatch (EchoWithout args) = undefined
    --innerMachine Pwd = undefined
    --innerMachine (Cd path) = undefined
    --innerMachine (Exit code) = putStrLn ("exit with code" <> code)




innerMachine [] = do
  v <- ask
  return (v, ())



--                  keys        values      keys    values
correctnessZip :: [String] -> [String] -> [(String, String)]
correctnessZip keys values
  | null keys && not (null values) = []
  | length keys < length values =             -- means that last key will storage all other values
    let diff = length keys - length values in
      let (ones, others) = splitAt (length keys - 1) values in
        let (backHead : backTail) = reverse keys in
          zip (reverse backTail) ones <> [(backHead, foldl (<>) "" $ fmap (" " <>) others)]
  | length keys > length values =             -- means that for free keys will be add default values
    let diff = length keys - length values in
      let defaultValues = replicate diff mempty in
        zip keys (values <> defaultValues)
  | otherwise = zip keys values

{-
correctnessZip ["a", "b", "c"] ["x", "y", "z"]
correctnessZip ["a", "b", "c"] ["x"]
correctnessZip ["a"] ["x", "y", "z"]
correctnessZip ["a", "b"] ["x", "y", "z"]
correctnessZip [] ["x", "y", "z"]
correctnessZip ["a", "b"] []
-}

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
  | null env = error "need execute with <full path too script> ...arguments"
  | otherwise =
    let scriptPath = head env in
      do
        content <- readFileParseToStatement scriptPath
        case content of
          Left _ -> error "fail while parse script"
          Right statements ->
            let arguments = Map.insert "0" scriptPath (addValuesToMap $ tail env) in
              let call = runReaderT (innerMachine statements)
                    MachineEnvironment {_declaredValues = arguments, _currentDirectory = ""} in
                      do
                        (state, _) <- call
                        print state

              --print statements
call = block1Execute ["/home/nikita/IdeaProjects/fp-homework-templates/hw3/blia.sh"]