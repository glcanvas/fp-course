{-# LANGUAGE RankNTypes #-}
module Block6 where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import Lens.Micro (Lens', Traversal', filtered, traversed, (^.), lens, (^..), (.~))
import Lens.Micro.Internal (each)

import Data.Functor.Identity (Identity(..))

import Data.List (isSuffixOf)

type FullPath = FilePath
type ShortPath = FilePath

data FS = Dir {name :: FilePath, contents :: [FS]} | File {name :: FilePath} deriving (Show, Eq)

getDirectory :: FullPath -> IO FS
getDirectory path = getDirectory' path mempty
  where
    getDirectory' :: FullPath -> ShortPath -> IO FS
    getDirectory' full short = do
      let combinePath = if short == mempty then full else full <> "/" <> short
      dir <- doesDirectoryExist combinePath
      if dir
        then buildForDir full short
        else buildForFile full short

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

getName :: FS -> FilePath
getName (Dir name' _) = name'
getName (File name') = name'

setName :: FS -> FilePath -> FS
setName (Dir _ ct) newName = Dir newName ct
setName (File _) newName = File newName

getContent :: FS -> [FS]
getContent (Dir _ ct) = ct
getContent (File _) = error "not content in File"

setContent :: FS -> [FS] -> FS
setContent (Dir name' _) = Dir name'
setContent (File _) = error "not content in File"

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

changeSuffix :: String -> String -> FS -> FS
changeSuffix begin after fs = runIdentity $
  (contentLens . traversed . filtered predicate . nameLens) (Identity . replaceFunc) fs
  where
    predicate :: FS -> Bool
    predicate fl@(File _) = begin `isSuffixOf` (fl ^. nameLens)
    predicate _ = False

    replaceFunc :: FilePath -> FilePath
    replaceFunc y = take (length y - length begin) y ++ after

fileList :: FS -> [FilePath]
fileList (File name') = pure name'
fileList fs = (fs ^. nameLens) : concatMap fileList (fs ^.. contentLens . traversed)

deleteDir :: FilePath -> FS -> FS
deleteDir path fs =
   (.~) contentLens (filter (not . predicate) (fs ^. contentLens)) fs
  where
    predicate :: FS -> Bool
    predicate dir@(Dir _ _) = dir ^. nameLens == path && null (dir ^. contentLens)
    predicate _ = False