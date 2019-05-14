module Main where

import Block1Bench
import Block2Bench
import Block4Bench

main :: IO ()
main = do
  evalBench
  perimetrBench
  squareBench
  concurrentReadCHT
  readCHT
  writeCHT
  concurrentReadCHT
  concurrentReadWriteCHT
  concurrentWriteCHT