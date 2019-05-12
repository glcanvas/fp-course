module Main where

import Block1Bench
import Block2Bench
import Block4Bench

main :: IO ()
main = concurrentReadWriteCHT --perimetrBench
  -- >> squareBench -- evalBench
