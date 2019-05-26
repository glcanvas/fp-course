module Block6 where

import System.Directory

type FullPath = FilePath
type ShortPath = FilePath

data FS
  = Dir {name :: FilePath, contents :: [FS]} | File {name :: FilePath} deriving (Show, Eq)

getDirectory :: FullPath -> IO FS
getDirectory path = getDirectory' path mempty
  where
    getDirectory' :: FullPath -> ShortPath -> IO FS
    getDirectory' full short = do
      let combinePath = if short == mempty then full else full <> "/" <> short
      putStrLn $ "exec with: " <> combinePath
      file <- doesFileExist combinePath
      dir <- doesDirectoryExist combinePath
      if file
        then buildForFile full short
        else
          if dir
            then buildForDir full short
            else error "ето шо такое?"

    buildForFile :: FullPath -> ShortPath -> IO FS
    buildForFile full short = do
      let combinePath = if short == mempty then full else short
      putStrLn $ "file: " <> full <> " : " <> short
      pure $ File {name = combinePath}

    buildForDir :: FullPath -> ShortPath -> IO FS
    buildForDir full short = do
      let combinePath = if short == mempty then full else short
      let fsCombinePath = if short == mempty then full else full <> "/" <> short
      putStrLn $ "path: " <> full <> " : " <> short
      content <- getDirectoryContents fsCombinePath
      let updWay =  filter (\x -> x /= "." && x /= "..") content
      listOfDirs <- mapM (getDirectory' fsCombinePath) updWay
      pure $ Dir {name = combinePath, contents = listOfDirs}
