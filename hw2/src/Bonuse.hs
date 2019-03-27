{-# LANGUAGE InstanceSigs #-}

module Bonuse (
  Cont(Cont)
) where

-- | Define continuation
newtype Cont r a = Cont
    {  -- | define cont constructor
      runCont :: (a -> r) -> r
    }

-- | implement functor for continuation
instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont v) =
    Cont (\r -> v (\a -> let inner = f a in r inner))

-- | implement applicative for continuation
instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont (\r -> r a)

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont  r b
  Cont func <*> Cont value = Cont (\r -> func (\f -> value (r . f )))

-- | implement Monad for continuation
instance Monad (Cont r) where
  return :: a -> Cont r a
  return a  = Cont (\f -> f a)

  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  Cont v >>= f = Cont (\r -> v (\a -> runCont (f a) r))
