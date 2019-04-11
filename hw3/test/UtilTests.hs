module UtilTests where


import Test.Hspec
import Data.Void
import Text.Megaparsec

import Util (AssignValue(..)
               , Statement(..)
               , Parser
               , parserAssign
               , parserAssignValue)

blockUtilTests :: IO ()
blockUtilTests = parserAssignValueTest

parserAssignValueTest :: IO()
parserAssignValueTest =
  hspec $
  describe "parserAssignValue" $ do
    it "normal test" $
      runParser parserAssignValue "test" "1234;" `shouldBe`
      (Right [Number 1234] :: Either (ParseErrorBundle String Void) [AssignValue])
    it "simple correct test" $
      runParser parserAssignValue "test" "$abs1234'privet'\"kek\";" `shouldBe`
      (Right [Pointer "abs1234", SingleQuote "privet", DoubleQuote "kek"] :: Either (ParseErrorBundle String Void) [AssignValue])
