{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
module Block3
(

) where

import Control.DeepSeq


newtype Reader e a = Reader { runReader :: (e -> a) }

instance Functor (Reader r) where
  fmap = undefined

instance Applicative (Reader r) where
  pure = undefined
  (<*>) = undefined

instance Monad (Reader e) where
  return a = Reader $ \e -> a

  (Reader r) >>= f = Reader $ \e ->
    let (Reader inner) = f (r e) in
      inner e

-- e -- enviroment
-- a -- value from this env

data Env = Env {osSystem::String, osVersion::Int, osBinary :: Int}

getOsName :: Reader Env String
getOsName = return "linux"






newtype Writer w a = Writer { runWriter :: (a,w) } deriving Show

instance Functor (Writer w) where
instance Applicative (Writer w) where

instance Monoid w => Monad (Writer w) where
  return :: a -> Writer w a
  return a = Writer (a, mempty)

  (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  (Writer (wa, ww)) >>= func =
    let Writer (b, w') = func wa in
      Writer (b, ww <> w')


divWithLog :: Int -> Writer String Int -> Writer String Int
divWithLog a (Writer (b, log)) =
  let result = Writer(a `div` b, "i'm div a by b\n") in
      result >>= (\v -> Writer(v, "hello call"))











newtype DList a = DList { unDL :: [a] -> [a] }

fromList :: [a] -> DList a
fromList l = DList (l++)

toList :: DList a -> [a]
toList (DList l) = l []


cons :: a -> DList a -> DList a
cons x (DList l) = DList (l [x] ++)

snoc :: DList a -> a -> DList a
snoc (DList l) x = DList (\v -> x : l v)

filter :: (a -> Bool) -> DList a -> DList a
filter f (DList l) = DList (Prelude.filter f (l []) ++ )



zip :: DList a -> DList b -> DList (a, b)
zip (DList a) (DList b)  = DList $ \x ->
  Prelude.zip (a []) (b []) ++ x


fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib 2 = 1
fib n = let !left = fib (n - 1) in
          let !right = fib(n - 2) in
            let !result = left + right in
              result


fib1 :: Int -> Integer
fib1 0 = 1
fib1 1 = 1
fib1 2 = 1
fib1 n = fib (n - 1) + fib (n - 2)


data Tree a
  = Branch (Tree a) a (Tree a)
  | Leaf a deriving Show

instance NFData a => NFData (Tree a) where
  rnf :: Tree a -> ()
  rnf (Leaf x) = x `seq` ()
  rnf (Branch l v r) =
    let a = rnf l in
      let b = rnf r in
        let c = rnf v in
        (a `seq` (c `seq` (b `seq` ())))

instance NFData a => NFData (DList a) where
  rnf :: DList a -> ()
  rnf (DList f) = f [] `seq` ()


newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State fun) = State
    (\ x ->
    let prevEval = fun x `seq` () in
      let (!a, !s) = fun x in
        let !b = f a in
        (b, s)
    )
{-
instance Applicative (State s) where
  pure :: a -> State s a
  pure a =
    let !evalA = a in
      State (\ s -> (evalA, let !innerS = s in innerS))

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State func) <*> (State val) =
    State
      (\x -> let (a, s) = val x in
        let b = func a in
          (b, s))

          -}