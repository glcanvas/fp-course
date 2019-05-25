{-# LANGUAGE RankNTypes #-}
module Block5 where

import Data.Functor.Identity
import Data.Functor.Const

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
-- => Lens' s s = forall f. Functor f => (a -> f a) -> s -> f s
type Lens' s a  = Lens s s a a

set  :: Lens' s a -> a -> s -> s
set lens a s = runIdentity $ lens (\_ -> Identity a) s

view :: Lens' s a -> s -> a
view lens object = getConst $ lens Const object

over :: Lens' s a -> (a -> a) -> s -> s
over lens func object = set lens (func $ view lens object) object

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
(^.) obj lens = view lens obj

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over


data A = A1 Int | A2 A Int