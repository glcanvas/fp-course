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

