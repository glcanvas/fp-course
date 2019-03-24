module Block2Test (
  block2Main
) where

import Block2 (eval, ArithmeticError(PowNegate, DivByZero), Expression(Const, Add, Sub, Mull, Div, Pow), moving)
import Test.Hspec

-- | combine all tests from this file
block2Main :: IO()
block2Main = do
  evalTest
  movingTest

-- | tests for eval
evalTest :: IO()
evalTest = hspec $
             describe "eval tests" $ do
               it "test1" $ eval (Const 5) `shouldBe` (Right 5 :: Either ArithmeticError Int)
               it "test2" $ eval (Sub (Const 5) (Const 6)) `shouldBe` (Right $ -1 :: Either ArithmeticError Int)
               it "test3" $ eval (Add (Pow (Const 5) (Const $ -4)) (Div (Const 2) (Const 0))) `shouldBe`
                (Left DivByZero :: Either ArithmeticError Int)

-- | tests for moving algorithm
movingTest :: IO()
movingTest =
  hspec $
  describe "moving tests" $ do
    it "test1" $ take 5 (moving 1000 [0 .. 10000]) `shouldBe` ([0, 0.5, 1, 1.5, 2.0] :: [Float])
    it "test2" $ moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` ([1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5] :: [Float])
    it "test3" $ moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` ([1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5] :: [Float])
    it "test4" $ moving 10000 [] `shouldBe` ([] :: [Float])
    it "test5" $ take 5 (moving 10000 [0 .. 10000]) `shouldBe` ([0, 0.5, 1, 1.5, 2.0] :: [Float])