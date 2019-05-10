{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module SimpleBlock2 (
    Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
) where

import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Control.Monad.ST

data Point = Point Int Int

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = (x1 * x2) + (y1 * y2)

crossProduct  :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = (x1 * y2) - (y1 * x2)

size :: Point -> Double
size (Point x y) =
  let value::Double = fromIntegral (x * x + y * y) in
    sqrt value

commonFunc :: Num a =>  ((Point, a) -> Point -> (Point, a)) -> [Point] -> a
commonFunc func = inner
  where
  --inner :: forall a . Num a =>  [Point] -> a
  inner [] = 0
  inner (x:xs) =
    let (_, y) = foldl func (x, 0) (xs ++ [x]) in
      y

perimeter :: [Point] -> Double
perimeter [] = 0
perimeter (x:xs) = commonFunc (\(ptr, per) a -> (a, per + edgeLen ptr a)) (x:xs)
  where
    edgeLen :: Point -> Point -> Double
    edgeLen p1 p2 = size $ minus p2 p1

doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea (x:xs) = commonFunc (\(ptr, per) a -> (a, per + edgeLen ptr a)) (x:xs)
  --  let (lastPt, res) = foldl' (\(ptr, per) a -> (a, per + edgeLen ptr a)) (x, 0) xs in
  --  res + edgeLen lastPt x
  where
    edgeLen :: Point -> Point -> Int
    edgeLen (Point x1 y1) (Point x2 y2) = (x1 - x2) * (y1 + y2)
