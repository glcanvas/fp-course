{-# LANGUAGE TemplateHaskell #-}

module Block1 (
  executeScript
) where

import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe(fromMaybe)
import Data.Void
import Text.Megaparsec

import System.Directory
import System.Process
import System.Exit
import Data.IORef

import Control.Lens
import Data.List.Split
import Data.Char(isSpace)
import DefineDataTypes
import UtilAssignValues
import Util
import UtilParserBase
import qualified Control.Exception as Except


makeLenses ''MachineEnvironment

-- | One of main function that interpret script values
innerMachine :: [Statement] -> ReaderT WrapMachineEnv IO ()
innerMachine [] = return ()

innerMachine (AssignRaw ptr value:xs) = do
  currentState <- ask
  machine <- lift $ readIORef (envValue currentState)
  let resolver = briefResolveAssignValue (machine^.declaredValues) value

  newMachine <- lift (newIORef machine)

  hardResolve <- lift $ hardResolveAssignValue (WrapMachineEnv newMachine) resolver
  let updMap = Map.insert ptr hardResolve (machine^.declaredValues)
  let updMachine = (declaredValues.~updMap) machine
  lift $ writeIORef (envValue currentState) updMachine
  local (const currentState) (innerMachine xs)

innerMachine (CustomCommand cmd:xs) = do
  currentState <- ask
  machine <- lift $ readIORef (envValue currentState)
  let resolver = briefResolveCommands (machine^.declaredValues) [cmd]
  (_, represent) <- lift $ hardResolveShellCommand currentState (head resolver)
  lift $ putStr represent
  if isExitCommand cmd
    then local (const currentState) (innerMachine [])
    else local (const currentState) (innerMachine xs)

innerMachine (ThreadCommand cmds:xs) = do
  currentState <- ask
  machine <- lift $ readIORef (envValue currentState)
  newMachine <- lift (newIORef machine)
  _ <- lift $ iterateWithArg (WrapMachineEnv newMachine) cmds
  local (const currentState) (innerMachine xs)

-- | full resolve assign value and representation all result command
--  with zero exit code as string and concatenate all in big string
hardResolveAssignValue :: WrapMachineEnv -> [AssignValue] -> IO String
hardResolveAssignValue e (SingleQuote x:xs) = pure x <> hardResolveAssignValue e xs
hardResolveAssignValue e (Pointer x:xs) =
  do
    me <- readIORef $ envValue e
    let resolver = Map.lookup x (me^.declaredValues) in
      pure (fromMaybe mempty resolver) <> hardResolveAssignValue e xs

hardResolveAssignValue e (AssignCommand cmd:xs) = do
  filterResult <- iterateWithArg e cmd
  let (_, sRep) = foldl (\(_, sB) (_, sA) -> (0::Integer, sB <> sA)) (0, mempty) filterResult
  pure sRep <> hardResolveAssignValue e xs

hardResolveAssignValue _ (DoubleQuote _:_) = error "not correct match"
hardResolveAssignValue _ [] = pure mempty

-- | execute external command and return exit code with result
hardResolveShellCommand :: WrapMachineEnv -> ShellCommands -> IO (Int, String)
hardResolveShellCommand e (InnerCommandConst cmd) = resolveInnerCommand e cmd
hardResolveShellCommand e (ExternalCommandConst cmd) = do
  v <- mapM (\x -> hardResolveAssignValue e [x]) (externalArguments cmd)
  solvedExternalName <- hardResolveAssignValue e (externalName cmd)
  let concatNameArgs = foldl (\b a -> b <> " " <> a) (solvedExternalName <> " ") v
  let splitedNameArgs = splitWhen isSpace concatNameArgs
  if null splitedNameArgs || null (head splitedNameArgs)
    then  pure (1, mempty)
    else do
      let name = head splitedNameArgs
      let args = filter (not . null) $ tail splitedNameArgs
      (exCode, out, err) <- Except.catch ( readCreateProcessWithExitCode (proc name args) "") errorHandler
      case exCode of
        ExitSuccess -> pure (0, out <> " " <> err)
        ExitFailure code -> pure (code, out <> " " <> err)

-- | resolve custom inner commands
resolveInnerCommand :: WrapMachineEnv -> InnerCommand -> IO (Int, String)
resolveInnerCommand e (Read args) = do
  (ioString, erCode) <- Except.catch (sequenceA (getLine, pure 0)) readHandler
  readString <- ioString
  me <- readIORef (envValue e)
  let zipped = correctnessZip args (words readString)
  let updMap = foldr (\(k, v) b -> Map.insert k v b) (me^.declaredValues) zipped
  writeIORef (envValue e) me{_declaredValues=updMap}
  pure (erCode, mempty)
  where
    readHandler :: Except.SomeException -> IO (IO String, Int)
    readHandler _ = pure (pure "", 1)

resolveInnerCommand e (Echo arguments) = do
  resolvedValues <- mapM (hardResolveAssignValue e) arguments
  if null resolvedValues
    then pure (0, mempty)
    else do
      let (deleteNewLine, filteredValue) = containsKey (head resolvedValues) "-n"
      if not (null resolvedValues) && not deleteNewLine
        then let concatValue = foldl (<>) "" $ map (<> " ") resolvedValues
              in pure (0, flatString concatValue <> "\n")
        else let concatValue = foldl (<>) "" $ map (<> " ") (filteredValue : tail resolvedValues)
              in pure (0, flatString concatValue)

resolveInnerCommand e Pwd = do
  me <- readIORef (envValue e)
  pure (0, me^.currentDirectory <> "\n")

resolveInnerCommand e (Cd way) = do
  me <- readIORef (envValue e)
  v <- hardResolveAssignValue e way
  let splited = splitOn "/" v
  let currentPath = splitOn "/" $ me^.currentDirectory
  let removedNull = filter (not . null) currentPath
  let path = mergePath splited removedNull
  a <- doesFileExist path
  b <- doesDirectoryExist path
  if a || b
    then do
    writeIORef (envValue e) $ (currentDirectory .~ path) me
    pure(0, mempty)
    else pure(1, mempty)
  where
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
    mergePath splited removedNull =
      if not (null splited) && null (head splited)
          then foldl (\b a -> b <> "/" <> a) mempty splited
          else foldl (\b a -> b <> "/" <> a) mempty (wrapper splited (reverse removedNull))

resolveInnerCommand _ (Exit code) = pure (read code, mempty)

-- | function that execute one "ShellCommand" and pass "MachineEnvironment" for other commands
iterateWithArg :: WrapMachineEnv -> [ShellCommands] -> IO [(Int, String)]
iterateWithArg e (x:xs) = do
  (errCode, stringResult) <- hardResolveShellCommand e x
  if isExitCommand x
    then pure []
    else do
      tailResult <- iterateWithArg e xs
      pure $ (errCode, stringResult) : tailResult

iterateWithArg _ [] = pure []

-- | check that command is Exit
isExitCommand :: ShellCommands -> Bool
isExitCommand (InnerCommandConst (Exit _)) = True
isExitCommand _ = False

-- | function for common call system process execute that handle error and return non zero code
errorHandler :: Except.SomeException -> IO (ExitCode, String, String)
errorHandler _ = pure (ExitFailure 1, mempty, mempty)

readFileParseToStatement :: FilePath -> IO (Either (ParseErrorBundle String Void) [Statement])
readFileParseToStatement path = do
  existFile <- doesFileExist path
  if existFile
    then do
      content <- readFile path
      let content' = content <> "\n"
      return $ runParser parserFile "" content'
    else
      putStrLn "file does not exist" >> return (Right [])


addValuesToMap :: [String] -> Map.Map String String
addValuesToMap array = wrappedCall array 1
  where
    wrappedCall :: [String] -> Int -> Map.Map String String
    wrappedCall (x:xs) pos =
      let inner = wrappedCall xs (pos + 1) in
      Map.insert (show pos) x inner
    wrappedCall [] _ = Map.empty

-- | main function that get arguments
-- and execute script
-- show error ig fail
-- write to console
--
-- >>> executeScript ["/fp-homework-templates/hw3/example.sh", "/LICENSE", "y"]
-- >>> will iterate throw all lines of example.sh script
executeScript :: [String] -> IO ()
executeScript e
  | null e = error "need execute with <full path too script> ...arguments"
  | otherwise =
    let scriptPath = head e in
      do
        content <- readFileParseToStatement scriptPath
        dir <- getCurrentDirectory
        case content of
          Left code -> putStrLn $ errorBundlePretty code
          Right statements ->
            let arguments = Map.insert "0" scriptPath (addValuesToMap $ tail e) in
              let meIO = newIORef MachineEnvironment {_declaredValues = arguments, _currentDirectory = dir} in
              do
                me <- meIO
                runReaderT (innerMachine statements) (WrapMachineEnv me)