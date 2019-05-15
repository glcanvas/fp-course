module Block1Bench (
  evalBench
) where

import qualified Block1 as B1
import qualified SimpleMult as SM

import Control.Monad.ST
import Data.STRef
import Control.Loop
import Criterion.Main

-- | generate matrix
matrixGenerate :: Int -> Int -> [[Int]]
matrixGenerate a b
  | a  > 0 && b > 0 = runST $ do
    emptyArray <- newSTRef []
    numLoop 0 (a - 1)
      (\_ -> do
         let generate = replicate b 0
         v <- readSTRef emptyArray
         writeSTRef emptyArray (generate : v))
    readSTRef emptyArray
  | otherwise = error "row/column must be more than 0"

-- | evaluate tests
evalBench :: IO ()
evalBench = defaultMain [
  bgroup "matrix mull par" matrixMultPar,
  bgroup "matrix mull single" matrixMult
  ]
  where
  matrixs =
    map (\x -> (x, matrixGenerate x x, matrixGenerate x x)) [100, 200, 250, 500]
  matrixMult =
    map (\(a, l, r) -> bench ("single "<> show a) $ nf (SM.multiply l) r) matrixs
  matrixMultPar =
    map (\(a, l, r) -> bench ("par "<> show a) $ nf (B1.multiply l) r) matrixs