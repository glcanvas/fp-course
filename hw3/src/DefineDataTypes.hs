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
  | Cd {cdArgument :: [AssignValue]}
  | Exit {exitCode :: String}
   deriving Show

data ExternalCommand = ExternalCommand
  {
    externalName :: String
    , externalArguments :: [String]
  } deriving (Show, Eq)

data AssignValue = SingleQuote String
  | DoubleQuote String
  | Pointer String
  | ExternalLinuxCommand ExternalCommand
  deriving (Show, Eq)


