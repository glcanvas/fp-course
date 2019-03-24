{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances#-}
{-# LANGUAGE FlexibleInstances#-}

module Block2Task3b(
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

-- | create MonadJoin from Monad
instance Monad m => MonadJoin m where
  returnJoin :: a -> m a
  returnJoin = return

  join :: m (m a) -> m a
  join mma = mma >>= id