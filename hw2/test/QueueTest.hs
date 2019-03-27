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
    it "queue size" $ queueSize (queuePush 1 queueEmpty) `shouldBe` (1 :: Int)
    it "queue pop on empty queue" $  queuePop queueEmpty `shouldBe` (Nothing :: Maybe (Queue Int, Int))
    it "queue pop on not empty queue" $  queuePop (queuePush 1 queueEmpty) `shouldBe` (Just (queueEmpty, 1) :: Maybe (Queue Int, Int))