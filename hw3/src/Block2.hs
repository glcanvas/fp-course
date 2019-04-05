{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Block2 where

import Control.Monad.Reader
import qualified Data.Map as Map
import Util

exec :: Statement -> IO (Map.Map String Expression, ())
exec st = runReaderT (runMachine st) Map.empty


runMachine :: Statement -> ReaderT (Map.Map String Expression) IO (Map.Map String Expression, ())
runMachine (Seq (x:xs)) = do
  currentMap <- ask
  (innerDeclare, unit) <- runMachine x
  let mergeMap = Map.union innerDeclare currentMap
  local (const mergeMap) (runMachine $ Seq xs)


runMachine (Seq []) = do
  lift $ putStrLn "end yse"
  return (Map.empty, ())

runMachine (Assign variable value) = do
  let mp = Map.insert variable value Map.empty
  v <- lift $ putStrLn variable
  return (mp, v)


--              input   output
blia :: Reader String Int
blia = do
  v <- ask
  return 1

blia' = runReader blia "1d"


call = exec (Seq [Assign "okk" (Var "qe")])