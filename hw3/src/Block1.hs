
module Block1 where

import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe(fromMaybe)
import Data.Void
import Text.Megaparsec
import Util
import Data.Char(ord)

runMachine :: Statement -> ReaderT (Map.Map String String) IO (Map.Map String String, ())
runMachine (Seq (x:xs)) = do
  currentMap <- ask
  (innerDeclare, unit) <- runMachine x
  let mergeMap = Map.union innerDeclare currentMap
  local (const mergeMap) (runMachine $ Seq xs)
runMachine (Seq []) = do
  lift $ putStrLn "end yse"
  currentMap <- ask
  return (currentMap, ())
runMachine (Assign variable value) = do
  let mp = Map.insert variable value Map.empty
  v <- lift $ putStrLn variable
  return (mp, v)

printOrd :: String -> IO ()
printOrd (x:xs) = do
  print (show (ord x) <> " " <> [x])
  printOrd xs

printOrd [] = pure ()

readFileParseToStatement :: FilePath -> IO (Either (ParseErrorBundle String Void) [Statement])
readFileParseToStatement path = do
  content <- readFile path
  let content' = content <> "\n"
  putStrLn content
  printOrd content
  return $ runParser parserFile "" content'
-- readFileParseToStatement "/home/nikita/IdeaProjects/fp-homework-templates/hw3/blia.sh"
