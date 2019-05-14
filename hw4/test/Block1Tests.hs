module Block1Tests (
  testCorrectnesParallel
  , testCorrectnesSingle
) where

import Test.Hspec

import qualified Block1 as B1
import qualified SimpleMult as SM

-- | common tests
commonTests :: ([[Int]] -> [[Int]] -> Maybe [[Int]]) -> IO ()
commonTests f =
  hspec $
    describe "matrix multiply" $ do
      it "single element" $
        f [[1]] [[1]] `shouldBe` (Just [[1]] :: Maybe [[Int]])
      it "not equal sizes" $
        f [[1, 2]] [[1]] `shouldBe` (Nothing :: Maybe [[Int]])
      it "two vectors" $
        f [[1, 2]] [[1], [1]] `shouldBe` (Just [[3]] :: Maybe [[Int]])
      it "vector matrix" $
        f [[1, 2, 3]] [[1, 1], [1, 1], [1,1]] `shouldBe` (Just [[6, 6]] :: Maybe [[Int]])
      it "vector matrix reverse" $
        f [[1, 1], [1, 1], [1,1]] [[1, 2, 3]] `shouldBe` (Nothing :: Maybe [[Int]])
      it "vector matrix incorrect" $
        f [[1], [2], [3]] [[1, 1], [1, 1], [1,1]] `shouldBe` (Nothing :: Maybe [[Int]])
      it "two square matrix" $
        f [[1, 1], [2, 2]] [[1, 1], [2, 2]] `shouldBe` (Just [[3, 3], [6, 6]] :: Maybe [[Int]])

-- | parallel thread test for mult
testCorrectnesParallel :: IO ()
testCorrectnesParallel = commonTests B1.multiply

-- | single thread test for mult
testCorrectnesSingle :: IO ()
testCorrectnesSingle = commonTests SM.multiply
