module Main (
  main
) where


import Test.Hspec
import UtilTests (blockUtilTests)
--import Block1Test (block1Main)
--import Block2Test (block2Main)
--import QueueTest (queueMain)
--import Block3Test (block3Main)

-- | combine all tests from blocks
main :: IO ()
main = do
  blockUtilTests
  --queueMain
  --block1Main
  --block2Main
  --block3Main