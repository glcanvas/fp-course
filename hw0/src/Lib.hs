{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (
    distributivity
  , associate
  , eitherAssoc
  , doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  , s
  , k
  , contraction
  , permutation
  , composition
  , identity
  , iterateElement
  , factorial
  , fibonacci
  , mapFix
  , zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  , taskSixFirst
  , taskSixSecond
  , taskSevenFirst
  , taskSevenSecond
  , taskSevenThird
)
where

import Data.Void (Void)
import Data.Function (fix)

-- | show that Either is distibutivity
distributivity:: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

-- | show that cortage is associative
associate :: (a, (b, c)) -> ((a, b), c)
associate (x,(y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

-- | show that Either is associative
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (h, g)
  where
      -- eitherAssoc help1
      h :: Either a (Either b c) -> Either (Either a b) c
      h (Left a) = Left (Left a)
      h (Right (Left b)) = Left (Right b)
      h (Right (Right c)) = Right c
      --eitherAssoc help2
      g :: Either (Either a b) c -> Either a (Either b c)
      g (Left (Left a)) = Left a
      g (Left (Right b)) = Right (Left b)
      g (Right c) = Right (Right c)

type Neg a = a -> Void

-- | proof that double negate is true
doubleNeg  :: a -> Neg (Neg a)
doubleNeg  a f = f a

-- | show that Glivenko theorem works
--
-- (((a | (a -> V)) -> V) -> V)
-- \!(a|!a) -> !(a|!a)a (\a -> )
-- notEitherANotA :: ((a | (a -> V)) -> V)
-- x = (\a -> notEitherANotA (Left a) :: Either a !a) :: a -> V
-- notEitherANotA x :: Void
--
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg notEitherANotA = notEitherANotA $ Right (notEitherANotA . Left)

-- | show that pierce not working
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | the same as pierce
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | show than third negate is true
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f x = f $ doubleNeg x

-- | S combinator
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | K combinator
k :: a -> b -> a
k = const

-- | proof that type of title equal type of term for composition
-- ocaml code for generate <https://github.com/glcanvas/type-theory_hw/blob/master/d.ml>
-- \f.\g.\a.f(g a)
-- |- (\f.(\g.(\a.(f (g a))))) : ((e->f)->((d->e)->(d->f))) [rule #3]
-- *   f : (e->f) |- (\g.(\a.(f (g a)))) : ((d->e)->(d->f)) [rule #3]
-- *   *   f : (e->f), g : (d->e) |- (\a.(f (g a))) : (d->f) [rule #3]
-- *   *   *   a : d, f : (e->f), g : (d->e) |- (f (g a)) : f [rule #2]
-- *   *   *   *   a : d, f : (e->f), g : (d->e) |- f : (e->f) [rule #1]
-- *   *   *   *   a : d, f : (e->f), g : (d->e) |- (g a) : e [rule #2]
-- *   *   *   *   *   a : d, f : (e->f), g : (d->e) |- g : (d->e) [rule #1]
-- *   *   *   *   *   a : d, f : (e->f), g : (d->e) |- a : d [rule #1]
composition :: (b -> c) -> (a -> b) -> a -> c
composition =
  s (s (k s) (s (s (k s) (s (k k) (k s))) (s (s (k s) (s (k k) (k k))) (s (k k) (s k k)))))
    (s (s (k s) (s (s (k s) (s (k k) (k s))) (s (s (k s) (s (k k) (k k))) (s (s (k s) (k k)) (k k)))))
       (s (s (k s) (s (s (k s) (s (k k) (k s))) (s (k k) (k k)))) (s (k k) (k k))))

-- | id in s k combinators
identity :: a -> a
identity = s k k

-- | proof that type of title equal type of term for contraction
-- ocaml code for generate <https://github.com/glcanvas/type-theory_hw/blob/master/d.ml>
-- \f.\a. (f a) a
--  |- (\f.(\a.((f a) a))) : ((c->(c->e))->(c->e)) [rule #3]
-- *   f : (c->(c->e)) |- (\a.((f a) a)) : (c->e) [rule #3]
-- *   *   a : c, f : (c->(c->e)) |- ((f a) a) : e [rule #2]
-- *   *   *   a : c, f : (c->(c->e)) |- (f a) : (c->e) [rule #2]
-- *   *   *   *   a : c, f : (c->(c->e)) |- f : (c->(c->e)) [rule #1]
-- *   *   *   *   a : c, f : (c->(c->e)) |- a : c [rule #1]
-- *   *   *   a : c, f : (c->(c->e)) |- a : c [rule #1]
contraction :: (a -> (a -> b)) -> a -> b
contraction = s (s (k s) (s (s (k s) (s (k k) (s k k))) (s (s (k s) (k k)) (k k)))) (s (s (k s) (k k)) (k k))

-- | proof that type of title equal type of term for permutation
-- ocaml code for generate <https://github.com/glcanvas/type-theory_hw/blob/master/d.ml>
-- \f.\a.\b. (f b) a
--  |- (\f.(\a.(\b.((f b) a)))) : ((d->(c->f))->(c->(d->f))) [rule #3]
-- *   f : (d->(c->f)) |- (\a.(\b.((f b) a))) : (c->(d->f)) [rule #3]
-- *   *   a : c, f : (d->(c->f)) |- (\b.((f b) a)) : (d->f) [rule #3]
-- *   *   *   a : c, b : d, f : (d->(c->f)) |- ((f b) a) : f [rule #2]
-- *   *   *   *   a : c, b : d, f : (d->(c->f)) |- (f b) : (c->f) [rule #2]
-- *   *   *   *   *   a : c, b : d, f : (d->(c->f)) |- f : (d->(c->f)) [rule #1]
-- *   *   *   *   *   a : c, b : d, f : (d->(c->f)) |- b : d [rule #1]
-- *   *   *   *   a : c, b : d, f : (d->(c->f)) |- a : c [rule #1]
permutation :: (a -> b -> c) -> b -> a -> c
permutation =
  s (s (k s)
       (s (s (k s) (s (k k) (k s)))
          (s (s (k s) (s (s (k s) (s (k k) (k s))) (s (s (k s) (s (k k) (k k))) (s (k k) (s k k)))))
             (s (s (k s) (s (s (k s) (s (k k) (k s))) (s (k k) (k k)))) (s (k k) (k k))))))
    (s (s (k s) (s (k k) (k k))) (s (s (k s) (k k)) (k k)))

-- | Explanation
--   w = fix w
--   x = w x
--   x :: int -> int
--   x = \y. if y == 0 then 1 else y * x (y - 1)
--   =>
--   let x = \y. if y == 0 then 1 else y * x (y - 1)
--         in x
--    3 ->
--        (\y. if y == 0 then 1 else y * x (y - 1)) 3
--        -> (3 * x 2) -> 3 * (\y. if y == 0 then 1 else y * x (y - 1)) 2 ->
--           3 * (2 * x 1) -> etc
--     ohh n = let x = \y -> if y == 0 then 1 else y * x (y - 1) in x n

-- | Iterate for array with fix point
iterateElement :: forall a . a -> [a]
iterateElement = fix w
      where
            w :: (a -> [a]) -> a -> [a]
            w f a = a : f a

-- | calculate factorial with fix point
factorial :: Integer -> Integer
factorial = fix w
      where
            w :: (Integer -> Integer) -> Integer -> Integer
            w _ 0 = 1
            w f n = n * f (n - 1)

-- | calculate fibbonacci number with fix point
fibonacci :: Integer -> Integer
fibonacci = fix w
      where
            w :: (Integer -> Integer) -> Integer -> Integer
            w _ 0 = 0
            w _ 1 = 1
            w _ 2 = 1
            w f n = f (n - 1) + f (n - 2)

-- | make mapping with fix point
mapFix :: forall a b . (a -> b) -> [a] -> [b]
mapFix func = fix w
      where
            w :: ([a] -> [b]) -> [a] -> [b]
            w _ [] = []
            w f (el:xs) =  func el : f xs

type Nat a = (a -> a) -> a -> a

-- | Zero in church numerals
zero :: Nat a
zero _ x = x

-- | Next number in church numerals
succChurch :: Nat a -> Nat a
succChurch num f x = num f (f x)

-- | Plus in church numerals
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

-- | Multiplication in church numerals
churchMult :: Nat a -> Nat a -> Nat a
churchMult n m f = n (m f)

-- | Church numerals to int
churchToInt :: Nat Integer  -> Integer
churchToInt num = num (+ 1) 0

-- | Representation of WHNF
-- explanation:
-- distributivity : Either a (b, c) -> (Either a b, Either a c)
-- distributivity (Left a) = (Left a, Left a)
-- \a -> Left a, Left a
taskSixFirst :: (Either String b, Either String c)
taskSixFirst = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | Representation of WHNF
-- will return False because null released with foldr
-- which before execute outer foldr return whole array of Just
-- after that will return False because "o" exist in String
--
-- foldr (\_ _ -> False) True (mapMaybe foo "pole chudes ochen' chudesno") - after rewrite null and $
taskSixSecond :: Bool
taskSixSecond = False

-- | Derive type for first expression in block seven
-- null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
taskSevenFirst :: Bool
taskSevenFirst = t5 (t0 t4 t6)  (t3 (t2 t1) t10)
  where
    t0 :: (b -> c) -> (a -> b) -> (a -> c)
    t0 = (.)
    t1 :: a -> a
    t1 = id
    t2 :: (a -> b -> c) -> (a, b) -> c
    t2 = uncurry
    t3 :: (a -> b) -> [a] -> [b]
    t3 = map
    t4 :: Foldable t => t a -> Bool
    t4 = null
    t5 :: (a -> b) -> a -> b
    t5 = ($)
    t6 :: [a] -> a
    t6 = head
    t7 :: [a] -> [a] -> [a]
    t7 = (++)
    t8 :: String -> String
    t8 = t7 "Dorian"
    t9 :: String
    t9 = "Grey"
    t10 :: [(String -> String, String)]
    t10 = [(t8, t9)]

-- | Derive type for second expression block seven
-- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
taskSevenSecond :: [(Integer, Integer)]
taskSevenSecond = t7
  where
    t0 ::  Either Integer Integer
    t0 = Left (1 + 2)
    t1 :: Either Integer Integer
    t1 = Right 64
    t2 :: [Either Integer Integer]
    t2 = [t0, t1]
    t3 :: [a] -> [b] -> [(a, b)]
    t3 = zip
    t4 :: [Either a b] -> [a]
    t4 [] = []
    t4 (Left x:xs) = x : t4 xs
    t4 (Right _:xs) = t4 xs
    t5 :: [Either a b] -> [b]
    t5 [] = []
    t5 (Right x:xs) = x : t5 xs
    t5 (Left _:xs) = t5 xs
    t6 :: [Either a b] -> [(a, b)]
    t6 x = t3 (t4 x) (t5 x)
    t7 :: [(Integer, Integer)]
    t7 = t6 t2

-- | Derive type for third expression block seven
-- let impl = \x y -> not x || y in
--    let isMod2 = \x -> x `mod` 2 == 0 in
--    let isMod4 = \x -> x `mod` 4 == 0 in
--    \x -> (isMod4 x) `impl` (isMod2 x)
taskSevenThird :: Integer -> Bool
taskSevenThird = t7
  where
    t0 :: Bool -> Bool
    t0 = not
    t1 :: Bool -> Bool -> Bool
    t1 = (||)
    t2 :: Bool -> Bool -> Bool
    t2 x y = t0 (t1 x y)
    t3 :: Integer-> Integer -> Integer
    t3 = mod
    t4 :: Integer -> Integer -> Bool
    t4 = (==)
    t5 :: Integer -> Bool
    t5 x = t4 (t3 x 2)  0
    t6 :: Integer -> Bool
    t6 x = t4 (t3 x 4) 0
    t7 :: Integer -> Bool
    t7 x = t2 (t5 x) (t6 x)