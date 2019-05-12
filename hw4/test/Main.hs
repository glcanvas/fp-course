module Main (
  main
) where


import Block1Tests
import Block2Tests
import Block4Tests
-- | combine all tests from blocks
main :: IO ()
main =
  testCorrectnesParallel
  >> testCorrectnesSingle
  >> perimeterTests
  >> squareTests
  >> testsCHT
  >> bigTestsCHT