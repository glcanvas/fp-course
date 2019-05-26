{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Block5 where

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
lens get set f s = set s <$> f (get s)

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 f s =
  case s of
    Left s1 -> Left <$> l1 f s1
    Right s2 -> Right <$> l2 f s2

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s =
  let kk = view' l s in
    let b' = f kk in
      let t' = set' l b' s in
        (b', t')
  where
    view' :: Lens s t a b -> s -> a
    view' lns object = getConst $ lns Const object
    set' :: Lens s t a b -> b -> s -> t
    set' l b s = runIdentity $ l (\_ -> Identity b) s