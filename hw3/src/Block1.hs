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

import DefineDataTypes
import UtilAssignValues
import Util
import qualified Control.Exception as Except

import Control.Applicative.Combinators

makeLenses ''MachineEnvironment

concatBriefResolvedValues :: [AssignValue] -> ReaderT MachineEnvironment IO (MachineEnvironment, ())
concatBriefResolvedValues values = undefined

innerMachine :: [Statement] -> ReaderT MachineEnvironment IO (MachineEnvironment, ())
innerMachine [] = ask >>= (\v -> return (v, ()))

innerMachine (AssignRaw ptr value:xs) = do
  currentState <- ask
  -- resolver consist only "SingleQuote" and brief resolved "ShellCommands"
  let resolver = briefResolveAssignValue (currentState^.declaredValues) value
  lift $ print "============="
  lift $ print resolver
  hardResolve <- lift $ hardResolveAssignValue currentState resolver
  lift $ putStrLn hardResolve
  let updMap = Map.insert ptr hardResolve (currentState^.declaredValues)
  local (declaredValues.~updMap) (innerMachine xs)


-- | full resolve assign value and representation all result command
--  with zero exit code as string and concatenate all in big string
hardResolveAssignValue :: MachineEnvironment -> [AssignValue] -> IO String
hardResolveAssignValue me (SingleQuote x:xs) = pure x <> hardResolveAssignValue me xs
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

resolveInnerCommand :: MachineEnvironment -> InnerCommand -> IO (Int, String, MachineEnvironment)
resolveInnerCommand me (Read args) = undefined


{-
innerMachine (CustomCommand (InnerCommandConst cmd):xs) = resolveInnerCommand cmd
  where
    resolveInnerCommand :: InnerCommand -> ReaderT MachineEnvironment IO (MachineEnvironment, ())
    resolveInnerCommand (Read args) = do
      currentState <- ask
      readString <- lift getLine
      let zipped = correctnessZip args (words readString)
      let updMap = foldr (\(k, v) b -> Map.insert k v b) (currentState^.declaredValues) zipped
      local (declaredValues.~updMap) (innerMachine xs)

    resolveInnerCommand (Echo args) = do
      currentState <- ask
      lift $ putStrLn (commonPathEcho currentState args)
      local (const currentState) (innerMachine xs)

    resolveInnerCommand (EchoWithout args) = do
      currentState <- ask
      lift $ putStr (commonPathEcho currentState args)
      local (const currentState) (innerMachine xs)

    resolveInnerCommand Pwd = do
      currentState <- ask
      lift $ putStrLn (currentState^.currentDirectory)
      local (const currentState) (innerMachine xs)

    resolveInnerCommand (Exit code) = do
      currentState <- ask
      lift $ putStrLn ("exit with code " <> code)
      local (const currentState) (innerMachine [])

    resolveInnerCommand (Cd path) = undefined

    commonPathEcho :: MachineEnvironment -> [[AssignValue]] -> String
    commonPathEcho me args =
      let resolve = map (resolveAssignValue (me^.declaredValues)) args in
        foldl (<>) "" $ map (<> " ") resolve

innerMachine (ThreadCommand cmd:xs) = undefined
-}


-- | function that execute one "ShellCommand" and pass "MachineEnvironment" for other commands
iterateWithArg :: MachineEnvironment -> [ShellCommands] -> IO [(Int, String, MachineEnvironment)]
iterateWithArg me (x:xs) = do
  (errCode, stringResult, newEnv) <- hardResolveShellCommand me x
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

-- | function for common call system process execute that handle error and return non zero code
errorHandler :: Except.SomeException -> IO (ExitCode, String, String)
errorHandler _ = pure (ExitFailure 1, "", "oops")

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
          Left _ -> error "fail while parse script"
          Right statements ->
            let arguments = Map.insert "0" scriptPath (addValuesToMap $ tail env) in
              let call = runReaderT (innerMachine statements)
                    MachineEnvironment {_declaredValues = arguments, _currentDirectory = dir} in
                      do
                        (state, _) <- call
                        print state
                        putStrLn ""
                        putStrLn ""
                        putStrLn ""
                        print statements
-- |
call = block1Execute ["/home/nikita/IdeaProjects/fp-homework-templates/hw3/example.sh", "x", "y"]
