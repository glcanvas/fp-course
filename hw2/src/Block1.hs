{-# LANGUAGE InstanceSigs #-}

module Block1 (
  stringSum
  , Tree(Branch, Leaf)
  , NonEmpty(..)
) where


import Data.Char (isSpace)
import Text.Read (readMaybe)

-- | data for representation of  not empty list
data NonEmpty a
  = a :| [a] -- | constructor of not empty list
  deriving Show

-- | data for representation of Tree
data Tree a
  = Branch (Tree a) (Tree a) -- constructor for branch of tree
  | Leaf a -- constructor for leaf that contains single value
  deriving Show

-- | Function that return sum of splited 'String' by whitespace
--
--  >>> "1 2 3"
-- Just 5
--
-- >>> "1 hmm 3"
-- Nothing
stringSum :: String -> Maybe Int
stringSum s = sum <$> traverse readMaybe (filter (not . null) $ splitByWhitespace s)
  where
    splitByWhitespace :: String -> [String]
    splitByWhitespace string =
      let x :| xs = foldr cmp ([] :| []) string
       in x : xs
    cmp :: Char -> NonEmpty String -> NonEmpty String
    cmp item (x :| xs) =
      if isSpace item
        then [] :| (x : xs)
        else (item : x) :| xs

-- law : 1) fmap id == fmap
--       2) fmap (f . g) = ( fmap f . fmap g)
-- proof
--       1) a) fmap id (Leaf x) == Leaf (id x)
--              == Leaf x
--
--          b) fmap id (Branch l r) ==
--                  let fmap f l == l and fmap f r == r then
--                    Branch (id l) (id r) == Branch l r
--
--       2) a) fmap (f . g) (Leaf x) == Leaf $ (f . g) x == Leaf (f (g x))
--                (fmap f . fmap g) Leaf x == (fmap f (fmap g Leaf x)) ==
--                    fmap f (Leaf $ g x) == Leaf (f (g x))
--
--          b) let fmap f l == l and fmap f r == r then
--                 fmap (f . g) (Branch l r) == Branch  ((f . g) l) ((f . g) r) == Branch (f (g l)) (f (g r))
--                    (fmap f . fmap g) (Branch l r) == (fmap f (fmap g (Branch l r))) ==
--                      fmap f (Branch  (g l) (g r)) == Branch (f (g l)) (f (g r))
-- | Representation functor for Tree
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf $ f  x
  fmap f (Branch x y) = Branch (fmap f x) (fmap f y)

-- law : 1) pure id <*> v = v  | (id)
--      2) pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)   | (composition)
--      3) pure f <*> pure x ≡ pure (f x)   | (homomorphism)
--      4) u <*> pure y ≡ pure ($ y) <*> u   | (interchange)
--
--  proofs: 1)   a) Leaf id <*> Leaf x = Leaf $ id x == Leaf x
--           b)  Leaf id <*> Branch l r = Branch (Leaf id <*> l) == l по предположению индукции
--
--      2) (Leaf .) <*> u <*> v <*> w =
--
--           a) (Leaf .) <*> (Leaf f) <*> (Leaf g) <*> (Leaf x) ==
--                (Leaf f . ) <*> (Leaf g) <*> (Leaf x) ==
--                  (Leaf f . g) <*> (Leaf f) == Leaf (  f . g x) == Leaf ( f (g x ) )
--              (Leaf f) <*> ((Leaf g) <*> (Leaf x)) == (Leaf f) <*> (Leaf g x)
--                == Leaf f (g x)
--             Branch f1 f2  <*> ((Branch g1 g2) <*> (Leaf x)) ==
--                Branch f1 f2 <*> (Branch (g1 <*> Leaf x) (g2 <*> Leaf x)) ==
--                  Branch (f1 <*> (g1 <*> Leaf x)) (f2 <*> (g2 <*> Leaf x))
--          b) (Leaf (.)) <*> (Branch f1 f2) <*> (Branch g1 g2) <*> (Leaf x) ==
--               Branch (f1 . ) (f2 .) <*> (Branch g1 g2) <*> (Leaf x) ==
--
--                Branch (f1 . g1) (f2 . g2) <*> Leaf x == Branch (f1 . (g1 x)) (f2 . (g2 x))
--             Branch f1 f2  <*> ((Branch g1 g2) <*> (Leaf x)) == Branch f1 f2 <*> (Branch (g1 x) (g2 x)) ==
--              Branch (f1 (g1 x)) (f2 (g2 x))
--         c) (Leaf (.)) <*> (Branch f1 f2) <*> (Branch g1 g2) <*> (Branch x1 x2) ==
--               Branch (f1 . ) (f2 .) <*> (Branch g1 g2) <*> (Branch x1 x2) ==
--                Branch (f1 . g1) (f2 . g2) <*> (Branch x1 x2) == Branch (f1 . (g1 x1)) (f2 . (g2 x2))
--            (Branch f1 f2) <*> ((Branch g1 g2) <*> (Branch x1 x2)) ==
--              (Branch f1 f2) <*> (Branch (g1 x1) (g2 x2)) == Branch (f1 (g1 x1)) (f2 (g2 x2))
--    3) Leaf f <*> Leaf x = Leaf (f x)
--
--    4) a) Leaf f <*> Leaf y  == Leaf (f y)
--          Leaf ( $ y) <*> Leaf f = Leaf (f $ y)
--
--       b) Branch f1 f2 <*> Leaf y == Branch (f1 <*> Leaf y) (f2 <*> Leaf y)
--          Leaf ( $ y) <*> Branch f1 f2 == Branch ( Leaf ( $ y) <*> f1 ) ( Leaf ( $ y) <*> f2)
--
--            т.к. база Leaf Leaf, которая доказана, то пункт б выполнен по предположению индукции
-- | Representation applicative for Tree
instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f <*> Leaf x = Leaf $ f x
  Leaf f <*> Branch l r = Branch (Leaf f <*> l) (Leaf f <*> r)
  Branch fl fr <*> Leaf x =  Branch (fl <*> Leaf x) (fr <*> Leaf x)
  Branch fl fr <*> Branch xl xr = Branch (fl <*> xl) (fr <*> xr)

-- | Representation foldable for Tree
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf x) = f x z
  foldr f z (Branch l r) = foldr f (foldr f z l) r

-- | Representation traversable for Tree
instance Traversable Tree where
  traverse f (Leaf x) = fmap Leaf (f x)
  traverse f (Branch l r) =
    let left = traverse f l in
      let right = traverse f r in
        Branch <$> left <*> right

-- | Representation functor for NonEmpty
instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| (f <$> xs)

-- | Representation applicative for NonEmpty
instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (f :| fs) <*> (x :| xs) = f x :| ( fs <*> xs)

-- | Representation foldable for NonEmpty
instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldr f z xs)

-- | Representation traversable for NonEmpty
instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) =  (:|) <$> f x <*> traverse f xs

