{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Block5 where

import Data.Functor.Identity
import Data.Functor.Const

--import Control.Lens

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
-- => Lens' s s = forall f. Functor f => (a -> f a) -> s -> f s
type Lens' s a  = Lens s s a a

set  :: Lens' s a -> a -> s -> s
set lns a s = runIdentity $ lns (\_ -> Identity a) s

view :: Lens' s a -> s -> a
view lns object = getConst $ lns Const object

over :: Lens' s a -> (a -> a) -> s -> s
over lns func object = set lns (func $ view lns object) object

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
(^.) obj lns = view lns obj

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

_1 :: Lens (a, x) (b, x) a b
_1 func (a, x) = (, x) <$> func a

_2 :: Lens (x, a) (x, b) a b
_2 func (x, a) = (x, ) <$> func a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = set s <$> f (get s)
