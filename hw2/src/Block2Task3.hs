{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances#-}
{-# LANGUAGE FlexibleInstances#-}

module Block2Task3 (
  MonadFish(returnFish, (>=>))
  , MonadJoin(returnJoin, join)
  , Monad(return, (>>=))
) where

import Prelude (const, id)

-- | Define class for Fish monad
class MonadFish m where
    -- | Function that make monad from value
    returnFish :: a -> m a
    -- | Function that build function a -> m c
    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

-- | Define class for Join monad
class MonadJoin m where
    -- | Function that make monad from value
    returnJoin :: a -> m a
    -- | Function that make join
    join :: m (m a) -> m a

-- | Define class for simple monad
class Monad m where
    -- | Function that make monad from value
    return :: a -> m a
    -- | Function that build new monad from monad and function that return monad
    (>>=)  :: m a -> (a -> m b) -> m b

-- | create MonadFish from Monad
instance Monad m => MonadFish m where
  returnFish :: a -> m a
  returnFish = return

  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  f >=> g = \x -> let mb = f x in
    let mc = (mb >>= g) in
      mc

-- | create Monad from MonadFish
instance MonadFish m => Monad m where
  return :: a -> m a
  return = returnFish

  (>>=) :: m a -> (a -> m b) -> m b
  x >>= f = (>=>) (const x) f x

-- | create MonadJoin from MonadFish
instance MonadFish m => MonadJoin m where
  returnJoin :: a -> m a
  returnJoin = returnFish

  join :: m (m a) -> m a
  join mma = (const mma >=> id) mma