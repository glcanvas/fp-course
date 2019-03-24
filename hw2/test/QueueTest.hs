module QueueTest (
  queueMain
) where

import Queue (Queue (Queue), queueSize, queuePush, queuePop, queueEmpty)
import Test.Hspec

-- | all tests for queue
queueMain :: IO()
queueMain =
  hspec $
  describe "queue tests" $ do
    it "test1" $ queueSize (queuePush 1 queueEmpty) `shouldBe` (1 :: Int)
    it "test2" $  queuePop queueEmpty `shouldBe` (Nothing :: Maybe (Queue Int, Int))
    it "test3" $  queuePop (queuePush 1 queueEmpty) `shouldBe` (Just (queueEmpty, 1) :: Maybe (Queue Int, Int))