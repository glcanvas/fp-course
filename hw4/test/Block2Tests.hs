module Block2Tests (
    perimeterTests
  , squareTests
) where

import Block2
import Test.Hspec

-- | tests for perimetr
perimeterTests :: IO ()
perimeterTests =
  hspec $
    describe "calculate perimetr" $ do
      it "single element" $
        perimeter [Point 0 0] `shouldBe` (0 :: Double)
      it "empty" $
        perimeter [] `shouldBe` (0 :: Double)
      it "two elements" $
        perimeter [Point 0 0, Point 1 0] `shouldBe` (2 :: Double)
      it "square" $
        perimeter [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` (4 :: Double)
      it "line" $
        perimeter (map (Point 0) [0 .. 100]) `shouldBe` (200 :: Double)

-- | tests for square
squareTests :: IO ()
squareTests =
  hspec $
    describe "calculate square" $ do
      it "single element" $
        doubleArea [Point 0 0] `shouldBe` (0 :: Int)
      it "empty" $
        doubleArea [] `shouldBe` (0 :: Int)
      it "two elements" $
        doubleArea [Point 0 0, Point 1 0] `shouldBe` (0 :: Int)
      it "square" $
        doubleArea [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` (2 :: Int)
      it "line" $
        doubleArea (map (\x -> Point x x) [0 .. 100]) `shouldBe` (0 :: Int)