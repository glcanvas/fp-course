{-# LANGUAGE RankNTypes #-}
module Block6 where

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import Lens.Micro -- (Lens', Lens, Traversal', filtered, traversed, (^.), lens)
import Lens.Micro.Internal -- (each)
import Lens.Micro.Extras
import Lens.Micro.Type

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
      pure $ File {name = combinePath}

    buildForDir :: FullPath -> ShortPath -> IO FS
    buildForDir full short = do
      let combinePath = if short == mempty then full else short
      let fsCombinePath = if short == mempty then full else full <> "/" <> short
      content <- getDirectoryContents fsCombinePath
      let updWay =  filter (\x -> x /= "." && x /= "..") content
      listOfDirs <- mapM (getDirectory' fsCombinePath) updWay
      pure $ Dir {name = combinePath, contents = listOfDirs}

-- | getter for name lens
getName :: FS -> FilePath
getName (Dir name _) = name
getName (File name) = name

-- | setter for name lens
setName :: FS -> FilePath -> FS
setName (Dir _ ct) newName = Dir newName ct
setName (File _) newName = File newName


getContent :: FS -> [FS]
getContent (Dir _ ct) = ct

-- | setter for content lens
setContent :: FS -> [FS] -> FS
setContent (Dir name _) = Dir name

nameLens :: Lens' FS FilePath
nameLens = lens getName setName

contentLens :: Lens' FS [FS]
contentLens = lens getContent setContent


cd :: FilePath -> Traversal' FS FS
cd path func = contentLens $ traversed (filtered predicate func)
  where
    predicate :: FS -> Bool
    predicate dir@(Dir _ _) = dir ^. nameLens == path
    predicate _ = False

ls :: Traversal' FS FilePath
ls func = contentLens (each $ nameLens func)

file :: FilePath -> Traversal' FS FilePath
file path func = contentLens $ traversed (filtered predicate (nameLens func))
  where
    predicate :: FS -> Bool
    predicate fl@(File _) = fl ^. nameLens == path
    predicate _ = False