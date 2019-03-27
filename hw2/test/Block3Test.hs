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
    it "ok test" $ runParser ok "123" `shouldBe` (Just ((), "123") :: Maybe ((), String))
    it "eof test for not empty stream" $ runParser eof "123" `shouldBe` (Nothing :: Maybe ((), String))
    it "eof test for empty stream" $ runParser eof "" `shouldBe` (Just ((), "") :: Maybe ((), String))
    it "satisfy test correct value" $ runParser (satisfy (== '1')) "123" == (Just ('1', "23") :: Maybe (Char, String))
    it "satisfy test incorrest value" $ runParser (satisfy (== '1')) "" `shouldBe` (Nothing :: Maybe (Char, String))
    it "element test incorrect value" $ runParser (element 5) [1, 2, 3] `shouldBe` (Nothing :: Maybe (Int, [Int]))
    it "element  test correct value" $ runParser (element 5) [5, 1 ,2, 3] `shouldBe` (Just (5, [1, 2, 3]) :: Maybe (Int, [Int]))
    it "stream test normal" $ runParser (stream [1, 2]) [1 ,2, 3, 4, 5] == (Just ([1, 2], [3, 4, 5]) :: Maybe ([Int], [Int]))
    it "getOne test normal" $ runParser getOne [1 ,2, 3, 4, 5] `shouldBe` (Just (1, [2, 3, 4, 5]) :: Maybe (Int, [Int]))

-- | tests for parser brackets
bracketTests :: IO()
bracketTests =
  hspec $
  describe "test for bracket parser" $ do
    it "normal test" $ runParser parserBranchSeq "()()()" `shouldBe` (Just ((), "") :: Maybe ((), String))
    it "empty string should be Just" $ runParser parserBranchSeq "" `shouldBe` (Just ((), "") :: Maybe ((), String))
    it "incorrect string test" $ runParser parserBranchSeq "((" `shouldBe` (Nothing :: Maybe ((), String))
    it "incorrect string with correct substring" $ runParser parserBranchSeq "()(" == (Nothing :: Maybe ((), String))

-- | test for parser integer
integerTests :: IO()
integerTests =
  hspec $
  describe "test for integer parser" $ do
    it "not integer string shoulb be Nothing" $ runParser parserInteger "()()()" == (Nothing :: Maybe (Int, String))
    it "empty string shoulb be Nothing" $ runParser parserInteger "" `shouldBe` (Nothing :: Maybe (Int, String))
    it "only sign char - should be Nothing" $ runParser parserInteger "-" `shouldBe` (Nothing :: Maybe (Int, String))
    it "only sign char + should be Nothing" $ runParser parserInteger "+" `shouldBe` (Nothing :: Maybe (Int, String))
    it "correct test with 1 and other string" $ runParser parserInteger "+1q" == (Just (1, "q") :: Maybe (Int, String))
    it "normal test" $ runParser parserInteger "+14" `shouldBe` (Just (14, "") :: Maybe (Int, String))
    it "correct test with -1" $ runParser parserInteger "-1q" == (Just (-1, "q") :: Maybe (Int, String))
    it "test with leading zeros" $ runParser parserInteger "007" `shouldBe` (Just (7, "") :: Maybe (Int, String))
    it "correct test with 1" $ runParser parserInteger "+1-2-3" `shouldBe` (Just (1, "-2-3") :: Maybe (Int, String))

-- | test for parser fold sequence
foldTests :: IO()
foldTests =
  hspec $
  describe "test for fold parser" $ do
    it "incorrect input" $ runParser parserFold "()()()" `shouldBe` (Nothing :: Maybe ([[Int]], String))
    it "normal test" $ runParser parserFold "1, 1" `shouldBe` (Just ([[1]], "") :: Maybe ([[Int]], String))
    it "zero size array" $ runParser parserFold "1, 1, 0" == (Just ([[1], []], "") :: Maybe ([[Int]], String))
    it "stream haven't enouth elements" $ runParser parserFold "1, 1, 2" == (Nothing :: Maybe ([[Int]], String))
    it "not correct values" $ runParser parserFold "1, 1 hmmm" `shouldBe` (Nothing :: Maybe ([[Int]], String))
    it "forgot delimetr" $ runParser parserFold "1, 1, 2, 2 2" `shouldBe` (Nothing :: Maybe ([[Int]], String))
    it "normal test" $ runParser parserFold "1, 1, 2, 2, 2" == (Just ([[1], [2, 2]], "") :: Maybe ([[Int]], String))
    it "not correct input" $ runParser parserFold "+3,2,3,3,    2, 1, 211asddas" == (Nothing :: Maybe ([[Int]], String))
    it "not enouth elements" $ runParser parserFold "-1, 1, 2 ,        3" == (Nothing :: Maybe ([[Int]], String))
    it "a lot of whitespace" $ runParser parserFold "+1, 1, 1 ,          -1" ==
      (Just([[1], [-1]], "") :: Maybe ([[Int]], String))

