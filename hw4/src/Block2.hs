{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Block2 (
    Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
) where

import Data.List

-- | data type for point
data Point = Point Int Int

-- | point plus function
plus :: Point -> Point -> Point
plus (Point !x1 !y1) (Point !x2 !y2) = Point (x1 + x2) (y1 + y2)

-- | point minus function
minus :: Point -> Point -> Point
minus (Point !x1 !y1) (Point !x2 !y2) = Point (x1 - x2) (y1 - y2)

-- | scalar product for two points
scalarProduct :: Point -> Point -> Int
scalarProduct (Point !x1 !y1) (Point !x2 !y2) = (x1 * x2) + (y1 * y2)

-- | cross product for two points
crossProduct  :: Point -> Point -> Int
crossProduct (Point !x1 !y1) (Point !x2 !y2) = (x1 * y2) - (y1 * x2)

-- | size for point decart
size :: Point -> Double
size (Point !x !y) =
  let value::Double = fromIntegral (x * x + y * y) in
    sqrt value

-- | function that calculate perimetr
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

-- | function that calculate area
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea (x:xs) =
  let (lastPt, res) = foldl' (\(ptr, per) a -> (a, per + edgeLen ptr a)) (x, 0) xs in
    res + edgeLen lastPt x
  where
    edgeLen :: Point -> Point -> Int
    edgeLen (Point !x1 !y1) (Point !x2 !y2) = (x2 - x1) * (y1 + y2)