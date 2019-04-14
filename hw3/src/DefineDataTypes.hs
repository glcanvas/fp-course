module DefineDataTypes (
  AssignValue(..)
  , Statement(..)
  , InnerCommand(..)
) where

data Statement = Seq [Statement]
  | AssignRaw String [AssignValue]
  | Assign String String
  | CustomCommand InnerCommand
  deriving Show

data InnerCommand = Read {readArguments :: [String]}
  | EchoWithout {echoArgumentsWithout :: [[AssignValue]]}
  | Echo {echoArguments :: [[AssignValue]]}
  | Pwd
  | Cd {cdArgument :: FilePath}
  | Exit
   deriving Show

data AssignValue = Number Int
  | SingleQuote String
  | DoubleQuote String
  | Pointer String
  deriving (Show, Eq)


