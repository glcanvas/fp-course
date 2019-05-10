module Main (
  main
) where


import Block1Tests
import Block2Tests
-- | combine all tests from blocks
main :: IO ()
main =
  testCorrectnesParallel
  >> testCorrectnesSingle
  >> perimeterTests
  >> squareTests