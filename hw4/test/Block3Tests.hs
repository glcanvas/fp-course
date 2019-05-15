module Block3Tests (
    gaussTests
) where

import Block3
import Test.Hspec

gaussTests :: IO ()
gaussTests =
  hspec $
    describe "simple tests" $ do
      it "valid test" $
        gauss [[False, True], [True, False]] [True, True] `shouldBe` (Just [True, True] :: Maybe [Bool])
      it "valid test validate" $
        verifySolution [[False, True], [True, False]] [True, True] [True, True] `shouldBe` (True :: Bool)
      it "non solution test" $
        gauss [[True, True], [True, True]] [True, True] `shouldBe` (Nothing :: Maybe [Bool])
      it "third matrix size" $
        gauss [[True, True, True], [False, True, True], [False, False, True]]
          [True, True, True] `shouldBe` (Just [False, False, True] :: Maybe [Bool])
      it "validate test" $
        verifySolution [[True, True, True], [False, True, True], [False, False, True]]
          [False, False, True] [True, True, True] `shouldBe` (True :: Bool)