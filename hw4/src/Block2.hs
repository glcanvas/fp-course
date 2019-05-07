{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Block2 where



import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Data.List

data Point = Point Int Int

plus :: Point -> Point -> Point
plus (Point !x1 !y1) (Point !x2 !y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point !x1 !y1) (Point !x2 !y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point !x1 !y1) (Point !x2 !y2) = (+) (x1 * x2) $! (y1 * y2)

crossProduct  :: Point -> Point -> Int
crossProduct (Point !x1 !y1) (Point !x2 !y2) = (-) (x1 * y2) $! (y1 * x2)

size :: Point -> Double
size (Point !x !y) =
  let value::Double = fromIntegral (x * x + y * y) in
    sqrt value

perimeter :: [Point] -> Double
perimeter [] = 0
perimeter (x:xs) =
  let (lastPt, res) = foldl' (\(ptr, per) a -> (a, per + edgeLen ptr a)) (x, 0) xs in
    res + edgeLen lastPt x
  where
    edgeLen :: Point -> Point -> Double
    edgeLen p1 p2 =
      let !ptr = minus p2 p1 in
        size ptr

doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea (x:xs) =
  let (lastPt, res) = foldl' (\(ptr, per) a -> (a, per + edgeLen ptr a)) (x, 0) xs in
    res + edgeLen lastPt x
  where
    edgeLen :: Point -> Point -> Int
    edgeLen (Point !x1 !y1) (Point !x2 !y2) = (x1 - x2) * (y1 + y2)


array = map (\x -> Point x x ) [0..10000001]
bch = [bench "10^7" $ nf perimeter array]

main'' :: IO()
main'' = defaultMain [
    bgroup "perimetr" bch
    ]