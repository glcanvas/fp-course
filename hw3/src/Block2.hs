{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Block2 where

import Control.Monad.Reader
import Data.Maybe(fromMaybe)
import qualified Data.Map as Map
import Util

{-
exec :: Statement -> IO (Map.Map String AssignValue, ())
exec st = runReaderT (runMachine st) Map.empty

--                                                  AssignValue               AssignValue
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
-}
{-
resolveValues :: Map.Map String AssignValue -> [AssignValue] -> AssignValue
resolveValues _ (Number x) = Number x
resolveValues _ (SingleQuote x) = SingleQuote x
resolveValues valuesMap (Pointer x) =
  let resolver = Map.lookup x valuesMap in
    fromMaybe (error "pizdets") resolver

call = exec (Seq [Assign "okk" (Pointer "qe")])
-}

mainkek :: FilePath -> IO ()
mainkek p = do
  v <- readFile p
  putStrLn v


run :: IO ()
run = mainkek "/home/nikita/IdeaProjects/fp-homework-templates/hw3/blia.sh"

