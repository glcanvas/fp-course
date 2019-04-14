module DefineDataTypes (
  AssignValue(..)
  , Statement(..)
  , InnerCommand(..)
) where

data Statement = AssignRaw String [AssignValue]
  | CustomCommand InnerCommand
  deriving Show

data InnerCommand = Read {readArguments :: [String]}
  | EchoWithout {echoArgumentsWithout :: [[AssignValue]]}
  | Echo {echoArguments :: [[AssignValue]]}
  | Pwd
  | Cd {cdArgument :: FilePath}
  | Exit {exitCode :: String}
   deriving Show

data AssignValue = SingleQuote String
  | DoubleQuote String
  | Pointer String
  deriving (Show, Eq)


