module Block2Bench (
    perimetrBench
  , squareBench
) where

import qualified Block2 as B2
import qualified SimpleBlock2 as SB2
import Criterion.Main

bchPerimetrFast :: [Benchmark]
bchPerimetrFast = [bench "10^7 fast" $ nf B2.perimeter (map (\x -> B2.Point x x ) [0..10000001])]

bchPerimetrSlow :: [Benchmark]
bchPerimetrSlow = [bench "10^7 slow " $ nf SB2.perimeter (map (\x -> SB2.Point x x ) [0..10000001])]

bchSquareFast :: [Benchmark]
bchSquareFast = [bench "10^7 fast" $ nf B2.doubleArea (map (\x -> B2.Point x x ) [0..10000001])]

bchSquareSlow :: [Benchmark]
bchSquareSlow = [bench "10^7 slow " $ nf SB2.doubleArea (map (\x -> SB2.Point x x ) [0..10000001])]


perimetrBench :: IO ()
perimetrBench = defaultMain [
    bgroup "perimetr fast" bchPerimetrFast,
    bgroup "perimetr slow" bchPerimetrSlow]

squareBench :: IO ()
squareBench = defaultMain [
  bgroup "square fast" bchSquareFast,
  bgroup "square slow" bchSquareSlow]
