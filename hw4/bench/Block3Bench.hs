module Block3Bench (
    block3Bench
) where

import Criterion.Main
import Block3

generateMatrix :: Int -> [[Bool]]
generateMatrix n = mapM (\i -> replicate i False <> replicate (n - i) True) [0 .. (n - 1)]

generateVector :: Int -> [Bool]
generateVector n = replicate n True

gaussBench :: [Benchmark]
gaussBench = map (\i -> bench (show i) $ nf (gauss (generateMatrix i)) (generateVector i)) [100, 500, 1000, 2500]

block3Bench :: IO ()
block3Bench = defaultMain [
    bgroup "gauss"gaussBench
  ]