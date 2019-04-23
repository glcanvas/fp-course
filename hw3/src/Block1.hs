{-# LANGUAGE TemplateHaskell #-}

module Block1 where

import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe(fromMaybe)
import Data.Void
import Text.Megaparsec

import System.Environment(getArgs)
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

import Control.Applicative.Combinators


makeLenses ''MachineEnvironment

newtype ENV = ENV {envValue :: IORef MachineEnvironment}


innerMachine :: [Statement] -> ReaderT ENV IO ()
innerMachine [] = ask >>= (\v -> return ())

innerMachine (AssignRaw ptr value:xs) = do
  currentState <- ask
  machine <- lift $ readIORef (envValue currentState)
  let resolver = briefResolveAssignValue (machine^.declaredValues) value

  newMachine <- lift (newIORef machine)
  let env = ENV newMachine

  hardResolve <- lift $ hardResolveAssignValue env resolver
  let updMap = Map.insert ptr hardResolve (machine^.declaredValues)
  let updMachine = (declaredValues.~updMap) machine
  lift $ writeIORef (envValue currentState) updMachine
  local (const currentState) (innerMachine xs)


innerMachine (CustomCommand cmd:xs) = do
  currentState <- ask
  machine <- lift $ readIORef (envValue currentState)
  let resolver = briefResolveCommands (machine^.declaredValues) [cmd]
  (exCode, represent) <- lift $ hardResolveShellCommand currentState (head resolver)
  lift $ putStr represent
  if isExitCommand cmd
    then local (const currentState) (innerMachine [])
    else local (const currentState) (innerMachine xs)

innerMachine (ThreadCommand cmds:xs) = do
  currentState <- ask
  machine <- lift $ readIORef (envValue currentState)
  let resolver = briefResolveCommands (machine^.declaredValues) cmds
  newMachine <- lift (newIORef machine)
  let env = ENV newMachine
  lift $ iterateWithArg env cmds
  local (const currentState) (innerMachine xs)



-- | full resolve assign value and representation all result command
--  with zero exit code as string and concatenate all in big string
hardResolveAssignValue :: ENV -> [AssignValue] -> IO String
hardResolveAssignValue me (SingleQuote x:xs) = pure x <> hardResolveAssignValue me xs
hardResolveAssignValue env (Pointer x:xs) =
  do
    me <- readIORef $ envValue env
    let resolver = Map.lookup x (me^.declaredValues) in
      pure (fromMaybe mempty resolver) <> hardResolveAssignValue env xs

hardResolveAssignValue env (AssignCommand cmd:xs) = do
  filterResult <- iterateWithArg env cmd
  let (_, sRep) = foldl (\(_, sB) (_, sA) -> (0, sB <> sA)) (0, mempty) filterResult
  pure sRep

hardResolveAssignValue _ [] = pure mempty




-- | execute external command and return exit code with result
hardResolveShellCommand :: ENV -> ShellCommands -> IO (Int, String)
hardResolveShellCommand env (InnerCommandConst cmd) = resolveInnerCommand env cmd
hardResolveShellCommand env (ExternalCommandConst cmd) = do
  v <- mapM (\x -> hardResolveAssignValue env [x]) (externalArguments cmd)
  solvedExternalName <- hardResolveAssignValue env (externalName cmd)
  let concatNameArgs = foldl (<>) solvedExternalName v
  let flatted = foldl (<>) [] (map splitStringByNewLine v)

  (exCode, out, err) <- Except.catch ( readCreateProcessWithExitCode (proc solvedExternalName flatted) "") errorHandler
  case exCode of
    ExitSuccess -> pure (0, out <> " " <> err)
    ExitFailure code -> pure (code, out <> " " <> err)




-- | resolve custom inner commands
resolveInnerCommand :: ENV -> InnerCommand -> IO (Int, String)
resolveInnerCommand env (Read args) = do
  (ioString, erCode) <- Except.catch (sequenceA (getLine, pure 0)) readHandler
  readString <- ioString
  me <- readIORef (envValue env)
  let zipped = correctnessZip args (words readString)
  let updMap = foldr (\(k, v) b -> Map.insert k v b) (me^.declaredValues) zipped
  writeIORef (envValue env) me{_declaredValues=updMap}
  pure (erCode, mempty)
  where
    readHandler :: Except.SomeException -> IO (IO String, Int)
    readHandler _ = pure (pure "", 1)

resolveInnerCommand me (Echo arguments) = do
  resolvedValues <- mapM (hardResolveAssignValue me) arguments
  if null resolvedValues
    then pure (0, mempty)
    else do
      let (deleteNewLine, filteredValue) = containsKey (head resolvedValues) "-n"
      if not (null resolvedValues) && not deleteNewLine
        then let concatValue = foldl (<>) "" $ map (<> " ") resolvedValues
              in pure (0, flatString concatValue <> "\n")
        else let concatValue = foldl (<>) "" $ map (<> " ") (filteredValue : tail resolvedValues)
              in pure (0, flatString concatValue)


resolveInnerCommand env Pwd = do
  me <- readIORef (envValue env)
  pure (0, me^.currentDirectory <> "\n")

resolveInnerCommand env (Cd way) = do
  me <- readIORef (envValue env)
  v <- hardResolveAssignValue env way
  let splited = splitOn "/" v
  let currentPath = splitOn "/" $ me^.currentDirectory
  let filtered = filter (not . null) currentPath
  let path = mergePath splited filtered
  a <- doesFileExist path
  b <- doesDirectoryExist path
  if a || b
    then do
    writeIORef (envValue env) $ (currentDirectory .~ path) me
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
    mergePath splited filtered =
      if not (null splited) && null (head splited)
          then foldl (\b a -> b <> "/" <> a) mempty splited
          else foldl (\b a -> b <> "/" <> a) mempty (wrapper splited (reverse filtered))

resolveInnerCommand _ (Exit code) = pure (read code, mempty)






-- | function that execute one "ShellCommand" and pass "MachineEnvironment" for other commands
iterateWithArg :: ENV -> [ShellCommands] -> IO [(Int, String)]
iterateWithArg env (x:xs) = do
  (errCode, stringResult) <- hardResolveShellCommand env x
  if isExitCommand x
    then pure []
    else do
      tailResult <- iterateWithArg env xs
      pure $ (errCode, stringResult) : tailResult

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
              let meIO = newIORef MachineEnvironment {_declaredValues = arguments, _currentDirectory = dir} in
              do
                me <- meIO
                let env = ENV me
                runReaderT (innerMachine statements) env

call = block1Execute ["/home/nikita/IdeaProjects/fp-homework-templates/hw3/example.sh", "x", "y"]

flatString :: String -> String
flatString str = foldl (<>) mempty (map (<> " ") $ filter (not . null) (splitWhen isSpace str))

splitStringByNewLine :: String -> [String]
splitStringByNewLine = splitOn "\n"