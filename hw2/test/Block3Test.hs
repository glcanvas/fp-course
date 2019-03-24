module Block3Test (
  block3Main
) where

import Test.Hspec
import Block3 (Parser(Parser), runParser,
  ok, eof, satisfy, element, stream, getOne, parserBranchSeq, parserInteger, parserFold)

block3Main :: IO()
block3Main =  do
  simpleParserTests
  bracketTests
  integerTests
  foldTests

-- | tests for parser simple parsers
simpleParserTests :: IO()
simpleParserTests =
  hspec $
  describe "simple parsers tests" $ do
    it "test1" $ runParser ok "123" `shouldBe` (Just ((), "123") :: Maybe ((), String))
    it "test2" $ runParser eof "123" `shouldBe` (Nothing :: Maybe ((), String))
    it "test3" $ runParser eof "" `shouldBe` (Just ((), "") :: Maybe ((), String))
    it "test4" $ runParser (satisfy (== '1')) "123" `shouldBe` (Just ('1', "23") :: Maybe (Char, String))
    it "test5" $ runParser (satisfy (== '1')) "" `shouldBe` (Nothing :: Maybe (Char, String))
    it "test6" $ runParser (element 5) [1, 2, 3] `shouldBe` (Nothing :: Maybe (Int, [Int]))
    it "test7" $ runParser (element 5) [5, 1 ,2, 3] `shouldBe` (Just (5, [1, 2, 3]) :: Maybe (Int, [Int]))
    it "test8" $ runParser (stream [1, 2]) [1 ,2, 3, 4, 5] == (Just ([1, 2], [3, 4, 5]) :: Maybe ([Int], [Int]))
    it "test9" $ runParser getOne [1 ,2, 3, 4, 5] `shouldBe` (Just (1, [2, 3, 4, 5]) :: Maybe (Int, [Int]))

-- | tests for parser brackets
bracketTests :: IO()
bracketTests =
  hspec $
  describe "test for bracket parser" $ do
    it "test1" $ runParser parserBranchSeq "()()()" `shouldBe` (Just ((), "") :: Maybe ((), String))
    it "test2" $ runParser parserBranchSeq "" `shouldBe` (Just ((), "") :: Maybe ((), String))
    it "test3" $ runParser parserBranchSeq "((" `shouldBe` (Nothing :: Maybe ((), String))
    it "test4" $ runParser parserBranchSeq "()(" `shouldBe` (Nothing :: Maybe ((), String))

-- | test for parser integer
integerTests :: IO()
integerTests =
  hspec $
  describe "test for integer parser" $ do
    it "test1" $ runParser parserInteger "()()()" `shouldBe` (Nothing :: Maybe (Int, String))
    it "test2" $ runParser parserInteger "" `shouldBe` (Nothing :: Maybe (Int, String))
    it "test3" $ runParser parserInteger "-" `shouldBe` (Nothing :: Maybe (Int, String))
    it "test4" $ runParser parserInteger "+" `shouldBe` (Nothing :: Maybe (Int, String))
    it "test5" $ runParser parserInteger "+1q" `shouldBe` (Just (1, "q") :: Maybe (Int, String))
    it "test6" $ runParser parserInteger "+14" `shouldBe` (Just (14, "") :: Maybe (Int, String))
    it "test7" $ runParser parserInteger "-1q" `shouldBe` (Just (-1, "q") :: Maybe (Int, String))
    it "test8" $ runParser parserInteger "007" `shouldBe` (Just (7, "") :: Maybe (Int, String))
    it "test9" $ runParser parserInteger "+1-2-3" `shouldBe` (Just (1, "-2-3") :: Maybe (Int, String))

-- | test for parser fold sequence
foldTests :: IO()
foldTests =
  hspec $
  describe "test for fold parser" $ do
    it "test1" $ runParser parserFold "()()()" `shouldBe` (Nothing :: Maybe ([[Int]], String))
    it "test2" $ runParser parserFold "1, 1" `shouldBe` (Just ([[1]], "") :: Maybe ([[Int]], String))
    it "test3" $ runParser parserFold "1, 1, 0" `shouldBe` (Just ([[1], []], "") :: Maybe ([[Int]], String))
    it "test4" $ runParser parserFold "1, 1, 2" `shouldBe` (Nothing :: Maybe ([[Int]], String))
    it "test5" $ runParser parserFold "1, 1 hmmm" `shouldBe` (Nothing :: Maybe ([[Int]], String))
    it "test6" $ runParser parserFold "1, 1, 2, 2 2" `shouldBe` (Nothing :: Maybe ([[Int]], String))
    it "test7" $ runParser parserFold "1, 1, 2, 2, 2" `shouldBe` (Just ([[1], [2, 2]], "") :: Maybe ([[Int]], String))
    it "test8" $ runParser parserFold "+3,2,3,3,     2, 1, 211asddas" == (Nothing :: Maybe ([[Int]], String))
    it "test9" $ runParser parserFold "-1, 1, 2 ,         3" == (Nothing :: Maybe ([[Int]], String))
    it "test10" $ runParser parserFold "+1, 1, 1 ,         -13" == (Just([[1], [-13]], "") :: Maybe ([[Int]], String))

