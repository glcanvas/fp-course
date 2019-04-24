module UtilTestScripts (
  executeDir
) where

import System.Directory
import Data.String.Utils
import Block1

-- | Function that execute all scripts from directory recursive that end with *.sh and is file
-- executeDir "/home/nikita/IdeaProjects/georgee/haskell-itmo-2019-hw3/task1/"
executeDir :: FilePath -> IO()
executeDir = iterateFiles
  where
    isShFile :: FilePath -> IO Bool
    isShFile file = do
      v <- doesFileExist file
      if v && endswith ".sh" file
        then return True
        else return False

    dirContent :: FilePath -> IO [FilePath]
    dirContent path = do
      v <- doesDirectoryExist path
      if v
      then do
        files <- getDirectoryContents path
        let filtered = filter (not . startswith ".") files
        pure $ map (\x -> path <> "/" <> x) filtered
      else pure mempty

    iterateFiles :: FilePath -> IO ()
    iterateFiles path = do
      content <- dirContent path
      executeFile content

    executeFile :: [FilePath]-> IO ()
    executeFile (path:xs) =
      do
      result <- isShFile path
      if result
        then do
          putStrLn "=============================="
          putStrLn $ "begin execite file " <> path
          executeScript [path]
          putStrLn $ "end execute file " <> path
          executeFile xs
        else executeDir path >> executeFile xs
    executeFile [] = pure ()