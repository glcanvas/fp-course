{-# LANGUAGE TemplateHaskell #-}

module Block1 where

import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe(fromMaybe)
import Data.Void
import Text.Megaparsec
import Data.Char(ord)

import System.Environment(getArgs)
import System.Directory
import System.Process
import System.Exit

import Control.Lens
import Data.List
import Data.List.Split
import DefineDataTypes
import UtilAssignValues
import Util
import UtilParserBase
import qualified Control.Exception as Except

import Control.Applicative.Combinators


makeLenses ''MachineEnvironment

concatBriefResolvedValues :: [AssignValue] -> ReaderT MachineEnvironment IO (MachineEnvironment, ())
concatBriefResolvedValues values = undefined

innerMachine :: [Statement] -> ReaderT MachineEnvironment IO (MachineEnvironment, ())
innerMachine [] = ask >>= (\v -> return (v, ()))

innerMachine (AssignRaw ptr value:xs) = do
  currentState <- ask
  let resolver = briefResolveAssignValue (currentState^.declaredValues) value
  hardResolve <- lift $ hardResolveAssignValue currentState resolver
  let updMap = Map.insert ptr hardResolve (currentState^.declaredValues)
  local (declaredValues.~updMap) (innerMachine xs)

innerMachine (CustomCommand cmd:xs) = do
  currentState <- ask
  let resolver = briefResolveCommands (currentState^.declaredValues) [cmd]
  (exCode, represent, machineEnv) <- lift $ hardResolveShellCommand currentState (head resolver)
  lift $ putStr represent
  if isExitCommand cmd
    then local (const machineEnv) (innerMachine [])
    else local (const machineEnv) (innerMachine xs)

innerMachine (ThreadCommand cmds:xs) = do
  currentState <- ask
  let resolver = briefResolveCommands (currentState^.declaredValues) cmds
  lift $ iterateWithArg currentState cmds
  local (const currentState) (innerMachine xs)

-- | full resolve assign value and representation all result command
--  with zero exit code as string and concatenate all in big string
hardResolveAssignValue :: MachineEnvironment -> [AssignValue] -> IO String
hardResolveAssignValue me (SingleQuote x:xs) = pure x <> hardResolveAssignValue me xs
hardResolveAssignValue me (Pointer x:xs) =
    let resolver = Map.lookup x (me^.declaredValues) in
      pure (fromMaybe mempty resolver) <> hardResolveAssignValue me xs

hardResolveAssignValue me (AssignCommand cmd:xs) = do
  filterResult <- iterateWithArg me cmd
  let (_, sRep, _) = foldl (\(_, sB, mB) (_, sA, mA) -> (0, sB <> sA, mA)) (0, mempty, mempty) filterResult
  pure sRep

hardResolveAssignValue _ [] = pure mempty

-- | execute external command and return exit code with result
hardResolveShellCommand :: MachineEnvironment -> ShellCommands -> IO (Int, String, MachineEnvironment)
hardResolveShellCommand me (InnerCommandConst cmd) = resolveInnerCommand me cmd
hardResolveShellCommand me (ExternalCommandConst cmd) = do
  v <- mapM (\x -> hardResolveAssignValue me [x]) (externalArguments cmd)
  (exCode, out, err) <- Except.catch (readCreateProcessWithExitCode (proc (externalName cmd) v) "") errorHandler
  case exCode of
    ExitSuccess -> pure (0, out, me)
    ExitFailure code -> pure (code, err, me)

-- | resolve custom inner commands
resolveInnerCommand :: MachineEnvironment -> InnerCommand -> IO (Int, String, MachineEnvironment)
resolveInnerCommand me (Read args) = do
  readString <- getLine
  let zipped = correctnessZip args (words readString)
  let updMap = foldr (\(k, v) b -> Map.insert k v b) (me^.declaredValues) zipped
  pure (0, mempty,  me{_declaredValues=updMap})

resolveInnerCommand me (Echo arguments) = do
  resolvedValues <- mapM (hardResolveAssignValue me) arguments
  let (deleteNewLine, filteredValue) = containsKey (head resolvedValues) "-n"
  if not (null resolvedValues) && not deleteNewLine
    then
      let concatValue = foldl (<>) "" $ map (<> " ") resolvedValues in
      pure (0, concatValue <> "\n", me)
    else
      let concatValue = foldl (<>) "" $ map (<> " ") (filteredValue : tail resolvedValues) in
      pure (0, concatValue, me)

resolveInnerCommand me Pwd = pure (0, me^.currentDirectory <> "\n", me)

resolveInnerCommand me (Cd way) = do
  v <- hardResolveAssignValue me way
  let splited = splitOn "/" v
  let currentPath = splitOn "/" $ me^.currentDirectory
  let filtered = filter (not . null) currentPath
  if not (null splited) && null (head splited)
    -- | for absolute path such that /.... hello
    then pure(0, mempty, (currentDirectory .~ foldl (\b a -> b <> "/" <> a) mempty splited) me)
    -- | for relative path such that heh/..
    else pure(0, mempty, (currentDirectory .~ foldl (\b a -> b <> "/" <> a) mempty (wrapper splited (reverse filtered))) me)
  where
    -- |       readedPath   envPath
    wrapper :: [String] -> [String] -> [String]
    wrapper [] ys = reverse ys
    wrapper (x:xs) [] =
      case x of
        "." -> wrapper xs []
        ".." -> wrapper xs []
        value -> wrapper xs [value]
    wrapper (x:xs) (y:ys) =
      case x of
        "." -> wrapper xs (y:ys)
        ".." -> wrapper xs ys
        value -> wrapper xs (value:y:ys)


resolveInnerCommand me (Exit code) = pure (read code, mempty, me)


-- | function that execute one "ShellCommand" and pass "MachineEnvironment" for other commands
iterateWithArg :: MachineEnvironment -> [ShellCommands] -> IO [(Int, String, MachineEnvironment)]
iterateWithArg me (x:xs) = do
  (errCode, stringResult, newEnv) <- hardResolveShellCommand me x
  if isExitCommand x
    then pure []
    else
      do
        tailResult <- iterateWithArg newEnv xs
        if errCode /= 0
          then pure tailResult
          else pure $ (errCode, stringResult, newEnv) : tailResult

iterateWithArg _ [] = pure []

-- | Function that return true if function is external othervise false
isCommandExternal :: String -> IO Bool
isCommandExternal cmd = do
  (exCode, out, _) <- Except.catch (readCreateProcessWithExitCode (shell $ "type " <> cmd) "") errorHandler
  let splited = words out
  if length splited /= 3 || splited!!2 /= "is"
    then pure False
    else
      case exCode of
        ExitFailure _ -> pure False
        ExitSuccess -> pure True

-- | check that command is Exit
isExitCommand :: ShellCommands -> Bool
isExitCommand (InnerCommandConst (Exit _)) = True
isExitCommand _ = False

-- | function for common call system process execute that handle error and return non zero code
errorHandler :: Except.SomeException -> IO (ExitCode, String, String)
errorHandler _ = pure (ExitFailure 1, mempty, mempty)

-- |
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

-- |
addValuesToMap :: [String] -> Map.Map String String
addValuesToMap array = wrappedCall array 1
  where
    wrappedCall :: [String] -> Int -> Map.Map String String
    wrappedCall (x:xs) pos =
      let inner = wrappedCall xs (pos + 1) in
      Map.insert (show pos) x inner
    wrappedCall [] _ = Map.empty

-- |
block1Execute :: [String] -> IO ()
block1Execute env
  | null env = error "need execute with <full path too script> ...arguments"
  | otherwise =
    let scriptPath = head env in
      do
        content <- readFileParseToStatement scriptPath
        dir <- getCurrentDirectory
        case content of
          Left code -> putStrLn $ errorBundlePretty code
          Right statements ->
            print statements >>
            let arguments = Map.insert "0" scriptPath (addValuesToMap $ tail env) in
              let call = runReaderT (innerMachine statements)
                    MachineEnvironment {_declaredValues = arguments, _currentDirectory = dir} in
                      do
                        (state, _) <- call
                        print state

-- |
call = block1Execute ["/home/nikita/IdeaProjects/fp-homework-templates/hw3/example.sh", "x", "y"]
