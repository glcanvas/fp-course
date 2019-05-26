module Block5Test (
  lensCombineTests
) where

import Test.Hspec
import Block5

data A = A Int B deriving (Show, Eq)
newtype B = B Char deriving (Show, Eq)

lensCombineTests :: IO ()
lensCombineTests =
  lensTest >> lensSetGetTest >> hardLensTest

lensSetGetTest :: IO ()
lensSetGetTest =
  hspec $
  describe "pair test" $ do
    it "get first" $ view _1 ("a", "b") `shouldBe` ("a" :: String)
    it "get second" $ view _2 ("a", "b") `shouldBe` ("b" :: String)
    it "set first" $ set _1 "b" ("a", "b") `shouldBe` (("b", "b") :: (String, String))
    it "set second" $ set _2 "b" ("b", "a") `shouldBe` (("b", "b") :: (String, String))
    it "update first" $ over _1 (+ 1) (1, 1) `shouldBe` ((2, 1) :: (Int, Int))
    it "update second" $ over _2 (+ 1) (1, 1) `shouldBe` ((1, 2) :: (Int, Int))

lensTest :: IO ()
lensTest =
  hspec $
  describe "generate lens test" $ do
    it "view A Int" $ view lensAInt (A 1 (B 'a')) `shouldBe` (1 :: Int)
    it "set A Int" $ set lensAInt 2 (A 1 (B 'a')) `shouldBe` (A 2 (B 'a') :: A)
    it "B get Char" $ view (lensAB.lensBChar) (A 1 (B 'a')) `shouldBe` ('a' :: Char)
    it "B update Char" $
      set (lensAB.lensBChar) 'b' (A 1 (B 'a')) `shouldBe` (A 1 (B 'b') :: A)

hardLensTest :: IO ()
hardLensTest =
  hspec $
    describe "hard lens" $ do
      it "choosing Left" $ view lensChoose (Left (B 'a')) `shouldBe` ('a' :: Char)
      it "choosing Right" $ view lensChoose (Right (A 1 (B 'a'))) `shouldBe` ('a' :: Char)

lensAInt :: Lens A A Int Int
lensAInt = lens getAInt setAInt

lensAB :: Lens A A B B
lensAB = lens getAB setAB

lensBChar :: Lens B B Char Char
lensBChar = lens getBChar setBChar

lensChoose :: Lens (Either B A) (Either B A) Char Char
lensChoose = choosing lensBChar (lensAB . lensBChar)

getAInt :: A -> Int
getAInt (A x _) = x

setAInt :: A -> Int -> A
setAInt (A _ v) y = A y v

getAB :: A -> B
getAB (A _ v) = v

setAB :: A -> B -> A
setAB (A x _) = A x

getBChar :: B -> Char
getBChar (B x) = x

setBChar :: B -> Char -> B
setBChar (B _) = B

