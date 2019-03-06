{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Block5
where

-- | Function than return list of item in Maybe `box`
maybeConcat :: forall a . [Maybe [a]] -> [a]
maybeConcat = foldl inner []
  where
      inner :: [a] -> Maybe [a] -> [a]
      inner x Nothing = x
      inner x (Just y) = x ++ y

-- | Function that return pair of two arrays of item in Either `box`
eitherConcat :: forall a b . Monoid a => Monoid b => [Either a b] -> (a, b)
eitherConcat = foldl inner (mempty, mempty)
  where
      inner :: (a, b) -> Either a b -> (a, b)
      inner (a, b) (Left x) = (a <> x, b )
      inner (a, b) (Right x) = (a, b <> x)

-- | Representation of not empty array
data NotEmpty a
  = a :| [a]
  deriving (Show)

-- | Realization of  semigroup for not empty array
instance Semigroup (NotEmpty a) where
  (<>) :: NotEmpty a -> NotEmpty a -> NotEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ [y] ++ ys)

-- | Representation of data that may store one or two
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show)

instance Semigroup (ThisOrThat a b) where
  -- | if return each time left of arguments that will be true for semigroup rule
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a  b
  (<>) (This a) (This _) = This a
  (<>) (That a) (That _) = That a
  (<>) (This a) (That b) = Both a b
  (<>) (That b) (This a) = Both a b
  (<>) (Both a b) (This _) = Both a b
  (<>) (Both a b) (That _) = Both a b
  (<>) (This a) (Both _ c) = Both a c
  (<>) (That a) (Both b _) = Both b a
  (<>) (Both a b) (Both _ _) = Both a b

--task2 hard
newtype Name
  = Name String
  deriving (Show)

instance Semigroup Name where
  (<>):: Name -> Name -> Name
  (<>) (Name "") (Name b) = Name b
  (<>) (Name a) (Name "") = Name a
  (<>) (Name a) (Name b) = Name (a  ++ "." ++ b)

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

newtype Endo a = Endo
  { appEndo :: a -> a
  }

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) (Endo f) (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id

--task3
data Builder = One Char | Many [Builder]
  deriving Show

instance Semigroup Builder where
  (<>) :: Builder -> Builder -> Builder
  (<>) (One x) (One y) = Many [One x, One y]
  (<>) (Many []) (One y) = One y
  (<>) (Many x) (One y) = Many $ x ++ [One y]
  (<>) (One x) (Many []) = One x
  (<>) (One x) (Many y) = Many $ One x : y
  (<>) (Many x) (Many y) = Many $ x ++ y

instance Monoid Builder where
  mempty :: Builder
  mempty = Many []

fromString :: String -> Builder
fromString = foldl (\b a -> b <> One a) mempty

toString :: Builder -> String
toString (One x) = [x]
toString (Many (One x : xs)) = let innerArray = toString (Many xs) in
  x : innerArray
toString (Many (Many x : xs)) = let innerArray = toString (Many xs) in
    let innerElem = toString (Many x) in
      innerElem ++ innerArray
toString (Many []) = mempty























