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
  ,
)
where

import Data.Void (Void)
import Data.Function (fix)

---- Task1
distributivity:: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associate :: (a, (b, c)) -> ((a, b), c)
associate (x,(y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

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

---- Task2
type Neg a = a -> Void


doubleNeg  :: a -> Neg (Neg a)
doubleNeg  a f = f a

ei :: Either a (Neg a)
ei = undefined

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = f ei

pierce :: ((a -> b) -> a) -> a
pierce = undefined

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f x = f $ doubleNeg x

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k = const

{-
\f.\g.\a.f(g a)
 |- (\f.(\g.(\a.(f (g a))))) : ((e->f)->((d->e)->(d->f))) [rule #3]
*   f : (e->f) |- (\g.(\a.(f (g a)))) : ((d->e)->(d->f)) [rule #3]
*   *   f : (e->f), g : (d->e) |- (\a.(f (g a))) : (d->f) [rule #3]
*   *   *   a : d, f : (e->f), g : (d->e) |- (f (g a)) : f [rule #2]
*   *   *   *   a : d, f : (e->f), g : (d->e) |- f : (e->f) [rule #1]
*   *   *   *   a : d, f : (e->f), g : (d->e) |- (g a) : e [rule #2]
*   *   *   *   *   a : d, f : (e->f), g : (d->e) |- g : (d->e) [rule #1]
*   *   *   *   *   a : d, f : (e->f), g : (d->e) |- a : d [rule #1]
-}
composition :: (b -> c) -> (a -> b) -> a -> c
composition =
  s (s (k s) (s (s (k s) (s (k k) (k s))) (s (s (k s) (s (k k) (k k))) (s (k k) (s k k)))))
    (s (s (k s) (s (s (k s) (s (k k) (k s))) (s (s (k s) (s (k k) (k k))) (s (s (k s) (k k)) (k k)))))
       (s (s (k s) (s (s (k s) (s (k k) (k s))) (s (k k) (k k)))) (s (k k) (k k))))

identity :: a -> a
identity = s k k

{-
\f.\a. (f a) a
 |- (\f.(\a.((f a) a))) : ((c->(c->e))->(c->e)) [rule #3]
*   f : (c->(c->e)) |- (\a.((f a) a)) : (c->e) [rule #3]
*   *   a : c, f : (c->(c->e)) |- ((f a) a) : e [rule #2]
*   *   *   a : c, f : (c->(c->e)) |- (f a) : (c->e) [rule #2]
*   *   *   *   a : c, f : (c->(c->e)) |- f : (c->(c->e)) [rule #1]
*   *   *   *   a : c, f : (c->(c->e)) |- a : c [rule #1]
*   *   *   a : c, f : (c->(c->e)) |- a : c [rule #1]
-}
contraction :: (a -> (a -> b)) -> a -> b
contraction = s (s (k s) (s (s (k s) (s (k k) (s k k))) (s (s (k s) (k k)) (k k)))) (s (s (k s) (k k)) (k k))

{-
\f.\a.\b. (f b) a
 |- (\f.(\a.(\b.((f b) a)))) : ((d->(c->f))->(c->(d->f))) [rule #3]
*   f : (d->(c->f)) |- (\a.(\b.((f b) a))) : (c->(d->f)) [rule #3]
*   *   a : c, f : (d->(c->f)) |- (\b.((f b) a)) : (d->f) [rule #3]
*   *   *   a : c, b : d, f : (d->(c->f)) |- ((f b) a) : f [rule #2]
*   *   *   *   a : c, b : d, f : (d->(c->f)) |- (f b) : (c->f) [rule #2]
*   *   *   *   *   a : c, b : d, f : (d->(c->f)) |- f : (d->(c->f)) [rule #1]
*   *   *   *   *   a : c, b : d, f : (d->(c->f)) |- b : d [rule #1]
*   *   *   *   a : c, b : d, f : (d->(c->f)) |- a : c [rule #1]
-}
permutation :: (a -> b -> c) -> b -> a -> c
permutation =
  s (s (k s)
       (s (s (k s) (s (k k) (k s)))
          (s (s (k s) (s (s (k s) (s (k k) (k s))) (s (s (k s) (s (k k) (k k))) (s (k k) (s k k)))))
             (s (s (k s) (s (s (k s) (s (k k) (k s))) (s (k k) (k k)))) (s (k k) (k k))))))
    (s (s (k s) (s (k k) (k k))) (s (s (k s) (k k)) (k k)))

---- Task4
iterateElement :: forall a . a -> [a]
iterateElement = fix w
      where
            w :: (a -> [a]) -> a -> [a]
            w f a = a : f a
{-
smth explanation
  w = fix w
  x = w x
  x :: int -> int
  x = \y. if y == 0 then 1 else y * x (y - 1)
  =>
  let x = \y. if y == 0 then 1 else y * x (y - 1)
        in x
   3 ->
       (\y. if y == 0 then 1 else y * x (y - 1)) 3
       -> (3 * x 2) -> 3 * (\y. if y == 0 then 1 else y * x (y - 1)) 2 ->
          3 * (2 * x 1) -> etc
    ohh n = let x = \y -> if y == 0 then 1 else y * x (y - 1) in x n
-}

factorial :: Integer -> Integer
factorial = fix w
      where
            w :: (Integer -> Integer) -> Integer -> Integer
            w _ 0 = 1
            w f n = n * f (n - 1)

fibonacci :: Integer -> Integer
fibonacci = fix w
      where
            w :: (Integer -> Integer) -> Integer -> Integer
            w _ 0 = 0
            w _ 1 = 1
            w _ 2 = 1
            w f n = f (n - 1) + f (n - 2)

mapFix :: forall a b . (a -> b) -> [a] -> [b]
mapFix func = fix w
      where
            w :: ([a] -> [b]) -> [a] -> [b]
            w _ [] = []
            w f (el:xs) =  func el : f xs

---- Task5
type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch num f x = num f (f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult n m f = n (m f)

churchToInt :: Nat Integer  -> Integer
churchToInt num = num (+ 1) 0

---- Task6



---- Task7

{-
. : (b -> c) -> (a -> b) -> (a -> c)
id : a -> a
uncurry : (a -> b -> c) -> (a, b) -> c
map : (a -> b) -> [a] -> [b]
null : t -> a -> Bool
$ : (a -> b) -> a -> b
head : [a] -> a
++ : [a] -> [a] -> [a]


b : String
c : String

uncurry id => a == b -> c  =>
uncurry id : (b -> c, b) -> c

map (uncurry id) [...] : ((b -> c, b) -> c) -> [(b -> c, b)] -> [c]

map (uncurry id) [((++) "Dorian ", " Grey")] = ["Dorian Grey"] : [String]
head $ map (uncurry id) [((++) "Dorian ", " Grey")] = "Dorian Grey" : String
null . head $ map (uncurry id) [((++) "Dorian ", " Grey")] = "False" : Bool

-}
task7_first = null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]


lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left x: xs) = x : lefts xs
lefts (Right x: xs) = lefts xs

rights :: [Either a b] -> [b]
rights [] = []
rights (Right x : xs) = x : rights xs
rights (Left x : xs) = rights xs

{-
  zip : [a] -> [b] -> [(a,b)]

  Left (1 + 2) : Either Integer b
  Right (2 ^ 6) : Either a Integer

  [Left (1 + 2), Right (2 ^ 6)] : [(Either a b, Either a b)]
  (\x -> zip (lefts x) (rights x)) : [Either a b] -> [(a, b)]
-}
task7_second = (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]

