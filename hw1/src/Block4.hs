{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Block4 (
    Pair
  , NotEmpty
  , Tree
  , splitOn
  , joinWith
)
where

--task1
data Pair a
  = Pair a a
  deriving (Show)

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair first second) = f first <> f second
  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f m (Pair first second) = f first (f second m)

--task1 hard
data NotEmpty a
  = a :| [a]
  deriving (Show)

instance Foldable NotEmpty where
  foldMap :: forall a m . Monoid m => (a -> m) -> NotEmpty a -> m
  foldMap f (x :| xs) = iter x xs
    where
        iter :: a -> [a] -> m
        iter a [] = f a
        iter a arr = let inner = iter (head arr) (tail arr) in
          f a <> inner
  foldr :: forall a b . (a -> b -> b) -> b -> NotEmpty a -> b
  foldr f n (x :| xs) = iter x xs
    where
        iter :: a -> [a] -> b
          iter a [] = f a n
            iter a arr = let inner = iter (head arr) (tail arr) in
              f a inner

data Tree a
  = Leaf
  | Branch [a] (Tree a) (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap :: forall m a . Monoid m => (a -> m) -> Tree a -> m
  foldMap func = innerCall
    where
        innerCall :: Tree a -> m
        innerCall Leaf = mempty
        innerCall (Branch x left right) = let innerLeft = innerCall left in
          let innerRight = innerCall right in
            innerLeft <> foldMap func x <> innerRight
  foldr :: forall a b . (a -> b -> b) -> b -> Tree a -> b
  foldr f = innerCall
    where
        innerCall :: b -> Tree a -> b
        innerCall v Leaf = v
        innerCall v (Branch x left right) = let innerRight = innerCall v right in
          let middle = foldr f innerRight x in
            let innerLeft = innerCall middle left in
              innerLeft

--task2
splitOn :: forall a . Eq a => a -> [a] -> NotEmpty [a]
splitOn pattern = foldl cmp ([] :| [])
  where
      cmp :: NotEmpty [a] -> a -> NotEmpty [a]
      cmp (x:|xs) item =
        if pattern == item
        then [] :| (xs ++ [x])
        else (x ++ [item]) :| xs

joinWith :: forall a . a -> NotEmpty [a] -> [a]
joinWith pattern (x:|xs) = foldl (\b a -> b ++ [pattern] ++ a) x xs