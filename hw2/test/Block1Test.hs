module Block1Test (
  block1Main
) where

import Block1(stringSum, Tree(Branch, Leaf))
import Test.Hspec

-- | tests for stringSum
block1Main :: IO()
block1Main = stringSumTest

stringSumTest :: IO()
stringSumTest =
  hspec $
  describe "stringToSum" $ do
    it "normal test" $ stringSum "1 2 3 4" `shouldBe` (Just 10 :: Maybe Int)
    it "test with empty string" $ stringSum "" `shouldBe` (Just 0 :: Maybe Int)
    it "test with one negate number" $ stringSum "1 -2" `shouldBe` (Just (-1) :: Maybe Int)
    it "test with all negate number" $ stringSum "-2 -2 " `shouldBe` (Just (-4) :: Maybe Int)
    it "incrrect input" $ stringSum "-2 -2 hmmm" `shouldBe` (Nothing:: Maybe Int)
    it "test with whitespace string" $ stringSum "          " `shouldBe` (Just 0:: Maybe Int)