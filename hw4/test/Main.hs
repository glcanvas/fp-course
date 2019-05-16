module Main (
  main
) where

import Block1Tests
import Block2Tests
import Block3Tests
import Block4Tests

-- | combine all tests from blocks
main :: IO ()
main =
  testCorrectnesParallel
  >> testCorrectnesSingle
  >> perimeterTests
  >> squareTests
  >> gaussTests
  >> testsCHT
  >> bigTestsCHT
  >> asyncExceptTestsCHT