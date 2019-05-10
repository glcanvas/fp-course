module Main where

import Block1Bench
import Block2Bench

main :: IO ()
main = perimetrBench
  >> squareBench -- evalBench
