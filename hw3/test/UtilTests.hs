module UtilTests where


import Test.Hspec
import Util (Expression(..)
               , Statement(..)
               , Parser
               , parserAssign
               , parserAssignValue)

blockUtilTests :: IO ()
blockUtilTests = undefined

{-
stringSumTest :: IO()
stringSumTest =
  hspec $
  describe "parserAssignValue" $ do
    it "normal test" $ stringSum "1 2 3 4" `shouldBe` (Just 10 :: Maybe Int)
    it "test with empty string" $ stringSum "" `shouldBe` (Just 0 :: Maybe Int)
    it "test with one negate number" $ stringSum "1 -2" `shouldBe` (Just (-1) :: Maybe Int)
    it "test with all negate number" $ stringSum "-2 -2 " `shouldBe` (Just (-4) :: Maybe Int)
    it "incrrect input" $ stringSum "-2 -2 hmmm" `shouldBe` (Nothing:: Maybe Int)
    it "test with whitespace string" $ stringSum "          " `shouldBe` (Just 0:: Maybe Int)
-}