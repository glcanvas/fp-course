{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Block5 (
    Lens
  , Lens'
  , set
  , view
  , over
  , (.~)
  , (^.)
  , (%~)
  , _1
  , _2
  , lens
  , choosing
  , (<%~)
  , (<<%~)
) where

import Data.Functor.Identity
import Data.Functor.Const

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

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
lens gt st f s = st s <$> f (gt s)

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 f s =
  case s of
    Left s1 -> Left <$> l1 f s1
    Right s2 -> Right <$> l2 f s2

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s =
  let b' = f (getConst $ l Const s) in
    let t' = runIdentity $ l (\_ -> Identity b') s in
      (b', t')

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s =
  let a' = getConst $ l Const s in
    let b' = f a' in
      let t' = runIdentity $ l (\_ -> Identity b') s in
        (a', t')