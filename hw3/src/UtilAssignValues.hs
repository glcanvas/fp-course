module UtilAssignValues (
    parserAssign
  , parserAssignValue
  , assignedExpression
  , parserPointer
  , briefResolveAssignValue
  , briefResolveCommands
) where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Text.Megaparsec.Char (string)

import UtilParserBase
import DefineDataTypes
import UtilCommands

-- | Parse such substring as
-- Variable=smth
-- where smth is String in single quote or number or other variable
parserAssign :: Parser Statement
parserAssign = do
  isSpaceWithoutEOL
  variable <- assignIdentifier
  single '='
  AssignRaw variable <$> parserEndOfCommand parserAssignValue

-- | Wrap parsed string in one of "AssignValue"
--
-- >>> $a$b$c"asdaasd"'adadsa'
-- [Pointer a, Pointer b ...]
parserAssignValue :: Parser [AssignValue]
parserAssignValue = some assignedExpression

-- | Function that resolve only pointers, double quotes and assign commands
-- in order to get array only consist with single pointer and functions that necessary evaluate
briefResolveAssignValue :: Map.Map String String -> [AssignValue] -> [AssignValue]
briefResolveAssignValue valueMap = innerCall
  where
    innerCall :: [AssignValue] -> [AssignValue]
    innerCall (SingleQuote x:xs) = SingleQuote x : innerCall xs
    innerCall (Pointer x:xs) = Pointer x : innerCall xs
    innerCall (DoubleQuote x:xs) = briefResolveAssignValue valueMap x <> innerCall xs
    innerCall (AssignCommand x:xs) = (AssignCommand $ briefResolveCommands valueMap x) : innerCall xs
    innerCall [] = mempty

-- | the same as upper function
briefResolveCommands :: Map.Map String String -> [ShellCommands] -> [ShellCommands]
briefResolveCommands valueMap = innerCall
  where
    innerCall :: [ShellCommands] -> [ShellCommands]
    innerCall (InnerCommandConst cmd:xs) = briefResolveInnerCommand valueMap cmd : innerCall xs
    innerCall (ExternalCommandConst cmd:xs) = briefResolveExternalCommand valueMap cmd : innerCall xs
    innerCall [] = mempty

-- | the same as upper function
briefResolveInnerCommand :: Map.Map String String -> InnerCommand -> ShellCommands
briefResolveInnerCommand _ (Read args) = InnerCommandConst $ Read args
briefResolveInnerCommand valueMap (Echo args) = InnerCommandConst $ Echo (briefResolveEcho valueMap args)
briefResolveInnerCommand _ Pwd = InnerCommandConst Pwd
briefResolveInnerCommand valueMap (Cd way) = InnerCommandConst $ Cd (briefResolveAssignValue valueMap way)
briefResolveInnerCommand _ (Exit key) = InnerCommandConst $ Exit key

-- | resolve external command
briefResolveExternalCommand :: Map.Map String String -> ExternalCommand -> ShellCommands
briefResolveExternalCommand valueMap (ExternalConst name args) =
  ExternalCommandConst $
    ExternalConst (briefResolveAssignValue valueMap name) (briefResolveAssignValue valueMap args)

-- | The same as all upper function
briefResolveEcho :: Map.Map String String -> [[AssignValue]] -> [[AssignValue]]
briefResolveEcho valueMap = map (briefResolveAssignValue valueMap)