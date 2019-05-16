module Main (
    main
) where

import Block1Bench
import Block2Bench
import Block3Bench
import Block4Bench

main :: IO ()
main = do
  evalBench
  perimetrBench
  squareBench
  block3Bench
  concurrentReadCHT
  readCHT
  writeCHT
  concurrentReadCHT
  concurrentReadWriteCHT
  concurrentWriteCHT