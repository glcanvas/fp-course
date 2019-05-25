{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Block1 where

import Control.Monad.State
import qualified Data.Map as MAP
import Data.Typeable

data ST mapValue resultValue = ST (MAP.Map Int mapValue) resultValue

data STRef x = STRef Int x

instance Functor (ST mapValue) where

instance Applicative (ST m) where


instance Monad (ST vl) where
  (>>=) :: forall a b . Typeable a => Typeable b => ST vl a -> (a -> ST vl b) -> ST vl b
  (ST mp cur) >>= func =
    let kk :: Maybe (STRef Int) = cast cur in
      undefined
