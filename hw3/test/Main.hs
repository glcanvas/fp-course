module Main (
  main
) where


import Test.Hspec
import UtilTests (blockUtilTests)

-- | combine all tests from blocks
main :: IO ()
main = blockUtilTests