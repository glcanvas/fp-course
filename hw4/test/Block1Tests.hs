module Block1Tests where

import Test.Hspec
import Control.Monad.ST
import Data.STRef
import Data.List
import Control.Loop
import Criterion.Main

import qualified Block1 as B1
import qualified SimpleMult as SM

matrixGenerate :: Int -> Int -> [[Int]]
matrixGenerate a b
  | a  > 0 && b > 0 = runST $ do
    emptyArray <- newSTRef []
    numLoop 0 (a - 1)
      (\i -> do
         let generate = replicate b 0
         v <- readSTRef emptyArray
         writeSTRef emptyArray (generate : v))
    readSTRef emptyArray
  | otherwise = error "asd"


matrixs =
  Prelude.map (\x -> (x, matrixGenerate x x, matrixGenerate x x)) [100, 200, 250, 500]

matrixMult =
  Prelude.map (\(a, l, r) -> bench ("single "<> show a) $ nf (SM.multiply l) r) matrixs

matrixMultPar =
  Prelude.map (\(a, l, r) -> bench ("par "<> show a) $ nf (B1.multiply l) r) matrixs


evalBench :: IO ()
evalBench = defaultMain [
  bgroup "matrix mull par" matrixMultPar,
  bgroup "matrix mull single" matrixMult
  ]