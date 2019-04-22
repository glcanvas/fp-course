module UtilTestScripts (
  executeDir
) where

import System.Directory
import Data.String.Utils
import Text.Megaparsec
import Util

-- | Function that execute all scripts from directory recursive that end with *.sh and is file
-- executeDir "/home/nikita/IdeaProjects/haskell-itmo-2019-hw3/task1/"
executeDir :: FilePath -> IO()
executeDir dir = do
  files <- getDirectoryContents dir
  iterateIO files
  where
    goIfDir :: FilePath -> IO Bool
    goIfDir file = doesDirectoryExist $ dir <> file
    goIfShFile :: FilePath -> IO Bool
    goIfShFile file = do
      v <- doesFileExist $ dir <> file
      if v && endswith ".sh" file
        then return True
        else return False
    iterateIO :: [FilePath] -> IO ()
    iterateIO (x:xs) =
      if startswith "." x
        then iterateIO xs
        else inner (x : xs)
      where
        inner (x:xs) = do
          result <- goIfDir x
          if result
            then executeDir (dir <> x) *> iterateIO xs
            else executeFile (dir <> x) *> iterateIO xs
    iterateIO [] = putStrLn $ "end of dir" <> dir
    executeFile :: FilePath -> IO ()
    executeFile file = do
      content <- readFile file
      let content' = content <> "\n"
      let value = runParser parserFile "" content'
      print value
      print file
      case value of
        Left _ -> error "errr"
        Right _ -> return ()