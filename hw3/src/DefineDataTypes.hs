{-# LANGUAGE InstanceSigs #-}

module DefineDataTypes (
  AssignValue(..)
  , Statement(..)
  , InnerCommand(..)
  , ShellCommands(..)
  , ExternalCommand(..)
  , MachineEnvironment(..)
  , WrapMachineEnv(..)
  , correctnessZip
) where

import qualified Data.Map as Map
import Data.IORef

-- | Main data type for parsed values
data Statement = AssignRaw String [AssignValue] -- constructor for raw assigned value
  | CustomCommand ShellCommands-- command that write not in $(..)
  | ThreadCommand [ShellCommands] -- this commands will execute in new enviroment between $(..)
  deriving (Show, Eq)

-- | Common data type for all commands such as inner and external
data ShellCommands = InnerCommandConst InnerCommand -- constructor for inner command
  | ExternalCommandConst ExternalCommand -- constructor for external command
  deriving (Show, Eq)

-- | data type for inner commands such as echo cd exit etc
data InnerCommand = Read {readArguments :: [String]} -- constructor for read
  | Echo {echoArguments :: [[AssignValue]]} -- constructor for echo
  | Pwd -- constructor for pwd
  | Cd {cdArgument :: [AssignValue]} -- constructor for cd
  | Exit {exitCode :: String} -- constructor for exit
   deriving (Show, Eq)

-- | data type for external commands (from os)
data ExternalCommand = ExternalConst
  { -- constructor inner external command
    externalName :: [AssignValue] -- na,e of command
    , externalArguments :: [AssignValue] -- list of arguments
  } deriving (Show, Eq)

-- | data for raw values that may be in assigned constructor
data AssignValue = SingleQuote String -- constructor for single quote
  | DoubleQuote [AssignValue] -- constructor for double quote
  | Pointer String -- constructor for pointer
  | AssignCommand [ShellCommands] -- this commands will execute in new enviroment between $(..)
  deriving (Show, Eq)

-- | data for machine state that storage map of declared values and storage current work directory
data MachineEnvironment = MachineEnvironment
  {
    _declaredValues :: Map.Map String String
    , _currentDirectory :: FilePath
  } deriving Show

-- | newtype that wrap "MachineEnvironment" in IORef
newtype WrapMachineEnv = WrapMachineEnv
  { -- reference (not JoJo) for storage "MachineEnvironment"
    envValue :: IORef MachineEnvironment
  }

-- | semigroup for machine state
instance Semigroup MachineEnvironment where
  (<>) :: MachineEnvironment -> MachineEnvironment -> MachineEnvironment
  MachineEnvironment{} <> (MachineEnvironment dv1 cd1) = MachineEnvironment dv1 cd1

-- | monoid for machine state
instance Monoid MachineEnvironment where
  mempty :: MachineEnvironment
  mempty = MachineEnvironment Map.empty mempty

-- | Function that make same as zip function but
--   assign special values for keys without pair
--                  keys        values      keys    values
correctnessZip :: [String] -> [String] -> [(String, String)]
correctnessZip keys values
  | null keys && not (null values) = []
  | length keys < length values =             -- means that last key will storage all other values
      let (ones, others) = splitAt (length keys - 1) values in
        let (backHead : backTail) = reverse keys in
          zip (reverse backTail) ones <> [(backHead, foldl (<>) "" $ fmap (" " <>) others)]
  | length keys > length values =             -- means that for free keys will be add default values
    let diff = length keys - length values in
      let defaultValues = replicate diff mempty in
        zip keys (values <> defaultValues)
  | otherwise = zip keys values