module DefineDataTypes (
  AssignValue(..)
  , Statement(..)
  , InnerCommand(..)
  , ShellCommands(..)
  , ExternalCommand(..)
  , MachineEnvironment(..)
  , correctnessZip
) where

import qualified Data.Map as Map
data Statement = AssignRaw String [AssignValue]
  | CustomCommand ShellCommands-- command that write not in $(..)
  | ThreadCommand [ShellCommands] -- this commands will execute in new enviroment between $(..)
  deriving (Show, Eq)

-- | Common data type for all commands such as inner and external
data ShellCommands = InnerCommandConst InnerCommand
  | ExternalCommandConst ExternalCommand
  deriving (Show, Eq)

-- | data type for inner commands such as echo cd exit etc
data InnerCommand = Read {readArguments :: [String]}
  | EchoWithout {echoArgumentsWithout :: [[AssignValue]]}
  | Echo {echoArguments :: [[AssignValue]]}
  | Pwd
  | Cd {cdArgument :: [AssignValue]}
  | Exit {exitCode :: String}
   deriving (Show, Eq)

-- | data type for external commands (from os)
data ExternalCommand = ExternalConst
  {
    externalName :: String
    , externalArguments :: [AssignValue]
  } deriving (Show, Eq)

data AssignValue = SingleQuote String
  | DoubleQuote [AssignValue]
  | Pointer String
  | AssignCommand [ShellCommands] -- this commands will execute in new enviroment between $(..)
  deriving (Show, Eq)

data MachineEnvironment = MachineEnvironment
  {
    _declaredValues :: Map.Map String String
    , _currentDirectory :: FilePath
  } deriving Show



-- | Function that make same as zio function but
--   assign special values for keys without pair
--                  keys        values      keys    values
correctnessZip :: [String] -> [String] -> [(String, String)]
correctnessZip keys values
  | null keys && not (null values) = []
  | length keys < length values =             -- means that last key will storage all other values
    let diff = length keys - length values in
      let (ones, others) = splitAt (length keys - 1) values in
        let (backHead : backTail) = reverse keys in
          zip (reverse backTail) ones <> [(backHead, foldl (<>) "" $ fmap (" " <>) others)]
  | length keys > length values =             -- means that for free keys will be add default values
    let diff = length keys - length values in
      let defaultValues = replicate diff mempty in
        zip keys (values <> defaultValues)
  | otherwise = zip keys values
{-
correctnessZip ["a", "b", "c"] ["x", "y", "z"]
correctnessZip ["a", "b", "c"] ["x"]
correctnessZip ["a"] ["x", "y", "z"]
correctnessZip ["a", "b"] ["x", "y", "z"]
correctnessZip [] ["x", "y", "z"]
correctnessZip ["a", "b"] []
-}