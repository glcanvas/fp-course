{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Block1 where



import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Applicative
import qualified Data.Map.Strict as Map

import Lens.Micro.TH
import Control.Lens.Getter(view)
--import Control.Lens.Setter
import Lens.Micro

import Text.Megaparsec
import Control.Applicative
import Control.Monad
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L











import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Catch (throwM, Exception)
import Text.Read (readMaybe)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)


data StackError = UnexpectedStack
  deriving Show

instance Exception StackError

qmain :: IO ()
qmain = runReaderT stackApp [] >>=
        putStrLn . ("Final stack " <>) . show

stackApp :: ReaderT [Int] IO [Int]
stackApp = do
  cmd <- liftIO getLine
  handleCommand cmd

handleCommand :: String -> ReaderT [Int] IO [Int]
handleCommand "Pop" = do
  st <- ask
  when (null st) $ throwM UnexpectedStack
  local tail stackApp

handleCommand "Add" = do
  st <- ask
  case st of
    a : b : rest ->
      local (const $ a + b : rest) stackApp
    _ -> throwM UnexpectedStack

handleCommand "Push"  = do
  v <- ask
  undefined
  --local (228 :) stackApp

handleCommand "Exit" = reader id









type Parser = Parsec Void String

pScheme :: Parser String
pScheme = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"

pChar :: Parser Char
pChar = char 'q'

data BashStream
  = ProgramArgument Int
  | Equation String BashStream



yuy = parse pScheme "hello" "http sdfsdfdsfsd"

{-

-}

class VirtualMachine a where





















































































data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Int, _y :: Int } deriving (Show)

makeLenses ''Atom
makeLenses ''Point


atom = Atom "C" (Point 3 4)
xx = view (point . x) atom
xx' = atom^.(point.x)

newxx = over (point.x) (+1)
newxx' = (point.x) %~ (+1)

atomX :: Lens' Atom Int
atomX = lens (\a -> let pt = _point a in _x pt) (\a b -> a {_point=(_point a) {_x = b} } )

atomY :: Lens' Atom Int
atomY = lens (\a -> let pt = _point a in _y pt) (\a b -> a {_point=(_point a) {_y = b} } )

reflect :: Atom -> Atom
reflect atom =
  let forx = over (point.x) (+0) atom in
    over (point.y) (+0) forx

move :: Point -> Atom -> Atom
move ptr atom =
  let forx = over (point.x) (+ view x ptr) atom in
    over (point.y) (+ view y ptr) forx








data Molecule = Molecule { _atoms :: [Atom]}


makeLenses ''Molecule


ml = Molecule [atom,atom,atom,atom,atom,atom,atom,atom]

toListOf' :: Molecule -> [Atom]
toListOf' (Molecule a) = a

over' :: (Molecule ->  Molecule) -> Atom -> Atom
over' = undefined
atomsTs :: Traversal' Molecule Atom
atomsTs = undefined



data Pair a = Pair a a

leftPair :: Lens' (Pair a) a
leftPair = lens (\x -> let Pair n _ = x in n) (\a b -> let Pair _ n = a in Pair b n)

rightPair :: Lens' (Pair a) a
rightPair = lens (\x -> let Pair _ n = x in n) (\a b -> let Pair n _ = a in Pair n b)


instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair a b) = Pair (f a) (f b)

instance Foldable Pair where

instance Traversable Pair where
  traverse :: Applicative f => (a -> f b) -> Pair a -> f (Pair b)
  traverse f p =
    let fl = f $ view leftPair p in
      let fr = f $ view rightPair p in
        Pair <$> fl <*> fr

incPair :: Pair Int
incPair = over traverse (+ 1) (Pair 1 2)



























kek :: String -> Maybe String
kek "a" = Just "ok"
kek _ = Nothing

main' ::  MaybeT IO ()
main' = do
  s <- lift getLine
  let v = kek s
  undefined

hmm :: Int -> Writer String Int -> Writer String Int
hmm l r = r >>= (\x -> writer (x + l, " privet "))

hmmCall :: Writer String Int
hmmCall = hmm 5 (writer (6, "heh "))


envGet :: Int -> Reader Int String -> String
envGet i r = runReader r i


type EnvMap = Map.Map Int String

env :: Reader Int String
env =
  reader
    (\x ->
       let Just y = (Map.lookup x createEnv <|> Just "netu")
        in y)

createEnv :: EnvMap
createEnv = Map.insert 1 "poka" Map.empty <> Map.insert 0 "privet" Map.empty <>
  Map.insert 2 "bliat' artem zaibal ignorit'" Map.empty


--          v     prev state
execute :: Int -> Writer String String
execute v = do
 let call = envGet v env
 writer (call, "call key " ++ show v ++ " value " ++ call)




