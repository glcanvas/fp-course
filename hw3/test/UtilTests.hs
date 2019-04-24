module UtilTests (
  blockUtilTests
) where


import Test.Hspec
import Data.Void
import Text.Megaparsec

import DefineDataTypes
import UtilCommands


blockUtilTests :: IO ()
blockUtilTests = do
  parserExternalCommandTests
  parserCommandInThreadTests
  parserDoubleQuoteTests
  correctnessZipTest

parserExternalCommandTests :: IO()
parserExternalCommandTests =
  hspec $
  describe "parserExternalCommand" $ do
    it
      "normal test"
      (runParser parserExternalCommand "test" "bek kek" `shouldBe`
       (Right (ExternalCommandConst $
        ExternalConst {externalName = [SingleQuote "bek"], externalArguments = [SingleQuote "kek"]})
          :: Either (ParseErrorBundle String Void) ShellCommands))
    it "a lot of whitespace" $
      runParser parserExternalCommand "test" "        bek -n kek      dek mek fec" `shouldBe`
      (Right (ExternalCommandConst $
        ExternalConst
          {externalName = [SingleQuote "bek"],
            externalArguments =
              [SingleQuote "-n", SingleQuote "kek", SingleQuote "dek", SingleQuote "mek", SingleQuote "fec"]})
                :: Either (ParseErrorBundle String Void) ShellCommands)

parserCommandInThreadTests :: IO()
parserCommandInThreadTests =
  hspec $
  describe "parserCommandInThread" $ do
    it
      "normal test"
      (runParser parserCommandInThread "test" "$(bek kek)" `shouldBe`
        (Right [ExternalCommandConst
          $ ExternalConst
            {externalName = [SingleQuote "bek"], externalArguments = [SingleQuote "kek"]}]
              :: Either (ParseErrorBundle String Void) [ShellCommands]))
    it
      "empty block test"
      (runParser parserCommandInThread "test" "$()" `shouldBe`
        (Right [] :: Either (ParseErrorBundle String Void) [ShellCommands]))
    it
      "a lot of whitespace"
      (runParser parserCommandInThread "test" "$(          a       )" `shouldBe`
        (Right [ExternalCommandConst $
          ExternalConst {externalName = [SingleQuote "a"], externalArguments = []}]
            :: Either (ParseErrorBundle String Void) [ShellCommands]))
    it
     "internal command read"
     (runParser parserCommandInThread "test" "$(read a b c;)" `shouldBe`
        (Right [InnerCommandConst $
          Read {readArguments = ["a", "b", "c"]}] :: Either (ParseErrorBundle String Void) [ShellCommands]))
    it
      "internal commands echo"
      (runParser parserCommandInThread "test" "$(echo  a         )" `shouldBe`
        (Right [InnerCommandConst $
          Echo {echoArguments = [[SingleQuote "a"]]}] :: Either (ParseErrorBundle String Void) [ShellCommands]))
    it
      "internal commands echo -n"
      (runParser parserCommandInThread "test" "$(echo  -n bek         ;)" `shouldBe`
        (Right [InnerCommandConst $ Echo {echoArguments = [[SingleQuote "-n"], [SingleQuote "bek"]]}]
          :: Either (ParseErrorBundle String Void) [ShellCommands]))
    it
      "command with \" "
      (runParser parserCommandInThread "test" "$(bek mek \"$file\" )" `shouldBe`
        (Right [ExternalCommandConst $
          ExternalConst {externalName = [SingleQuote "bek"],
            externalArguments = [SingleQuote "mek", DoubleQuote [Pointer "file"]]}]
              :: Either (ParseErrorBundle String Void) [ShellCommands]))

parserDoubleQuoteTests :: IO()
parserDoubleQuoteTests =
  hspec $
  describe "parserCommandInThread" $ do
    it
      "normal test"
      (runParser doubleQuote "test" "\" bek mek\"" `shouldBe`
        (Right [SingleQuote " bek mek"] :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "empty string"
      (runParser doubleQuote "test" "\"\"" `shouldBe`
        (Right [] :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "function test"
      (runParser doubleQuote "test" "\"$(bek mek)\"" `shouldBe`
        (Right [AssignCommand [ExternalCommandConst
          (ExternalConst {externalName = [SingleQuote "bek"], externalArguments = [SingleQuote "mek"]})]]
            :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "function test with whitespace"
      (runParser doubleQuote "test" "\"               $(      bek       mek       )          \"" `shouldBe`
        (Right [SingleQuote "               ",
          AssignCommand [ExternalCommandConst
            (ExternalConst {externalName = [SingleQuote "bek"], externalArguments = [SingleQuote "mek"]})],
              SingleQuote "          "] :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "two inner function function"
      (runParser doubleQuote "test" "\"$(   bek          mek;fek        lek       )\"" `shouldBe`
        (Right [AssignCommand
          [ExternalCommandConst
            (ExternalConst {externalName = [SingleQuote "bek"], externalArguments = [SingleQuote "mek"]}),
              ExternalCommandConst
                (ExternalConst {externalName = [SingleQuote "fek"], externalArguments = [SingleQuote "lek"]})]]
                  :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "function with pointer"
      (runParser doubleQuote "test" "\"$(bek mek;) $ptr\"" `shouldBe`
        (Right [AssignCommand
          [ExternalCommandConst
            (ExternalConst
              {externalName = [SingleQuote "bek"], externalArguments = [SingleQuote "mek"]})], SingleQuote " ", Pointer "ptr"]
                :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "pointers"
      (runParser doubleQuote "test" "\"$bek $ptr mek fek\"" `shouldBe`
        (Right [Pointer "bek", SingleQuote " ", Pointer "ptr", SingleQuote " mek fek"]
          :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "\\$ \\ \\\\ \""
      (runParser doubleQuote "test" "\" \\$bek  \\ \\\\ \\\"  \"" `shouldBe`
        (Right [SingleQuote " \\",Pointer "bek",SingleQuote "  \\ \\\\ \\"]
          :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "quotes in $() "
      (runParser doubleQuote "test" "\"$(bek \"$file\" $file) \"" `shouldBe`
        ( Right [AssignCommand [ExternalCommandConst
          (ExternalConst {externalName = [SingleQuote "bek"],
            externalArguments = [DoubleQuote [Pointer "file"],Pointer "file"]})],SingleQuote " "]
              :: Either (ParseErrorBundle String Void) [AssignValue]))
    it
      "recursive $()"
      (runParser doubleQuote "test" "\"$(bek \"$(mek $pek)\") \"" `shouldBe`
        ( Right [AssignCommand [ExternalCommandConst
          (ExternalConst {externalName = [SingleQuote "bek"],
            externalArguments = [DoubleQuote [AssignCommand
              [ExternalCommandConst (ExternalConst
                {externalName = [SingleQuote "mek"], externalArguments = [Pointer "pek"]})]]]})],SingleQuote " "]
                  :: Either (ParseErrorBundle String Void) [AssignValue]))

correctnessZipTest :: IO()
correctnessZipTest =
  hspec $
  describe "correctness for me zip" $ do
    it
      "normal test"
      (correctnessZip ["a", "b", "c"] ["x", "y", "z"] `shouldBe`
        ([("a", "x"), ("b", "y"), ("c", "z")] :: [(String, String)]))
    it
      "short values test"
      (correctnessZip ["a", "b", "c"] ["x"] `shouldBe`
        ([("a", "x"), ("b", ""), ("c", "")] :: [(String, String)]))
    it
      "short keys test"
      (correctnessZip ["a"] ["x", "y", "z"] `shouldBe`
        ([("a", " x y z")] :: [(String, String)]))
    it
      "short keys test"
      (correctnessZip ["a", "b"] ["x", "y", "z"] `shouldBe`
        ([("a", "x"), ("b", " y z")] :: [(String, String)]))
    it
      "empty keys test"
      (correctnessZip [] ["x", "y", "z"] `shouldBe`
        ([] :: [(String, String)]))
    it
      "empty values test"
      (correctnessZip ["a", "b", "c"] [] `shouldBe`
        ([("a", ""), ("b", ""), ("c", "")] :: [(String, String)]))