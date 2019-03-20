{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs#-}

module Block1 (
) where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) =  let first = min (min x y) z in
  let second = max (min x y) (max (min y z) (min x z)) in
    let third = max (max x y) z in
      (first, second, third)


data SM a = NTH | ONE a | BOTH a a

secondMax :: forall a . Ord a => [a] -> Maybe a
secondMax array =
            let result = foldr (\a b -> hmm b a) NTH array in
                case result of
                NTH -> Nothing
                (ONE _)-> Nothing
                (BOTH a _) -> Just a
            where
              hmm :: SM a -> a -> SM a
              hmm NTH x = ONE x
              hmm (ONE x) y = BOTH (min x y) (max x y)
              hmm (BOTH x y) z = let (_, b, c) = order3 (x, y, z) in BOTH b c




data Point3D a = Point3D a a a
  deriving Show


instance Functor Point3D where
  fmap :: (a -> b) -> Point3D a -> Point3D b
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

instance Applicative Point3D where
  pure:: a -> Point3D a
  pure x = Point3D x x x

  (<*>) :: Point3D (a -> b) -> Point3D a -> Point3D b
  (<*>) (Point3D f g h) (Point3D a b c) = Point3D (f a) (g b) (h c)

{-
data Tree a
  = Leaf (Maybe a)
  | Branch (Tree a) (Maybe a) (Tree a)

instance Functor Tree where
  fmap f (Leaf x) = Leaf $ f <$> x --fmap f x
  fmap f (Branch l Nothing r) = Branch (fmap f l) Nothing  (fmap f r)
  fmap f (Branch l (Just x) r) = Branch (fmap f l) (Just $ f x) (fmap f r)
-}
sfmp :: Functor f => a -> f b -> f a
sfmp a fb = fmap (const a) fb

fstappl :: Applicative f => f a -> f b -> f b
fstappl fa fb = let fmm1 = sfmp id fa in
                  (<*>) fmm1 fb

data Tree a
  = Leaf  a
  | Branch (Tree a) a ( Tree a)
  deriving (Show)


instance Functor Tree where
  fmap :: (a-> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left x right) = Branch (fmap f left) (f x) (fmap f right)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf x) = f x
  foldMap f (Branch l x r) = (foldMap f l) <> (f x) <> (foldMap f r)


instance Traversable Tree where
{-
т.к. реализован Traversable то Leaf x :: Functor Tree
-}
-- fmap :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<$) :: a -> f b -> f a
-- (*>) :: f a -> f b -> f b
-- Leaf :: a -> f a <=> a -> Tree a

  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x) = fmap Leaf (f x)
           --                  f a
           --     fmap :: (x -> y) -> f x -> f y
           --     Leaf :: (a -> Tree a)
  traverse f (Branch l x r) = let mdle = f x in
                                  let lft = traverse f l in
                                      let rgh = traverse f r in
                                          (<*>) ((<*>) (fmap Branch lft) mdle) rgh
          -- Branch :: Tree a -> a -> Tree a -> Tree a
          -- mdle :: f b
          -- lft :: f (Tree b)
          -- rgh :: f (Tree b)
          -- fmap Branch lft :: f (b -> Tree b -> Tree b)


--sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
traverse1 :: Applicative f => (a -> f a) -> [a] -> f [a]
traverse1 f array = sequenceA $ fmap f array
-- fmap = map
-- map :: (a -> b) -> a -> b


sequenceA1 :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA1 tfa = traverse (\x -> x) tfa


newtype Path a = Path [String] deriving Show

data Abs
data Rel


createRel:: String -> Maybe (Path Abs)
createRel s
  | "/" `isPrefixOf` s = Nothing
  | otherwise =
    let split = splitOn "/" s  in
      Just $ Path split


class HMM a where
  heh :: a -> String
  heh _ = "1"

instance HMM Int where
  heh _ = "1"


instance HMM [a] where
  heh :: [a] -> String
  heh _ = "1"

{-
data The a = This a | Those a | That a

instance Functor The where
  fmap :: (a -> b) -> The  a -> The b
  fmap f (This x) = This $ f x
  fmap f (Those x) = Those $ f x
  fmap f (That x) = That $ f x


instance Applicative The where
  pure :: a -> The a
  pure = Those

  (<*>) :: The (a -> b) -> The a -> The b
  (This f) <*> b = fmap f  b
  (That f) <*> b = fmap f  b
  (Those f) <*> b = fmap f  b
-}

data Post = Post {pTitle :: String, pBody :: String}

data Blog = Blog [Post] Int

bPosts :: Blog -> [Post]
bPosts (Blog x _) = x

bCounter :: Blog -> Int
bCounter (Blog _ x) = x

readPost :: Int -> Blog -> (Post, Blog)
readPost i blg = (bPosts blg !! i, Blog (bPosts blg) (bCounter blg + 1))

newPost :: Post -> Blog -> ((), Blog)
newPost p blog = ((), Blog  ( p : bPosts blog) (bCounter blog))

read12AndNew :: Blog -> Blog
read12AndNew blog =
  let (post1, blog') = readPost 1 blog
      ((), blog'') = newPost (Post "Bla" "<text>") blog'
      (post2, blog''') = readPost 2 blog''
   in blog'''


{-
readPost' :: Int -> BlogT Post
readPost' i = BlogT (readPost i)

newPost' :: Post -> BlogT ()
newPost' p = BlogT (newPost p)
-}

newtype BlogT a = BlogT (Blog -> (a, Blog))

readPost' :: Int -> BlogT Post
readPost' i = BlogT (readPost i)

newPost' :: Post -> BlogT ()
newPost' p = BlogT (newPost p)

instance Functor BlogT where
  fmap :: (a -> b) -> BlogT a -> BlogT b
  fmap f (BlogT innerF) =
    let hmm = (\x -> let res = innerF x in
                        let a' = f $ fst res in
                          let blog' = snd res in
                            (a', blog')) in
      BlogT hmm

emm = let x y = (1,2) in
    x


data Mb a = J a | N deriving (Show)

instance Functor Mb where
  fmap :: (a -> b) -> Mb a -> Mb b
  fmap f (J x) = J $ f x
  fmap f N = N

instance Applicative Mb where
  pure  = J

  (<*>) :: Mb (a -> b) -> Mb a -> Mb b
  J f <*> x = fmap f x
  N <*> _ = N

instance Monad Mb where
  return = pure
  (>>=) :: Mb a -> (a -> Mb b) -> Mb b
  J x >>= f = f x
  N >>= _ = N


