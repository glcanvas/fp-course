module Block1Test (
  block1Main
) where

import Block1(stringSum)
import Test.Hspec

-- | tests for stringSum
block1Main :: IO()
block1Main =
  hspec $
  describe "stringToSum" $ do
    it "test1" $ stringSum "1 2 3 4" `shouldBe` (Just 10 :: Maybe Int)
    it "test2" $ stringSum "" `shouldBe` (Just 0 :: Maybe Int)
    it "test3" $ stringSum "1 -2" `shouldBe` (Just (-1) :: Maybe Int)
    it "test4" $ stringSum "-2 -2 " `shouldBe` (Just (-4) :: Maybe Int)
    it "test5" $ stringSum "-2 -2 hmmm" `shouldBe` (Nothing:: Maybe Int)
    it "test6" $ stringSum "          " `shouldBe` (Just 0:: Maybe Int)