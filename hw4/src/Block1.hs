{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict#-}

module Block1 (
  multiply
  ) where

import Control.Monad.ST
import Data.STRef
import Data.List

import Control.DeepSeq

import Criterion.Main

import Control.Parallel
import Control.Parallel.Strategies
import qualified Control.Foldl as F

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply m1 m2 =
  let !m1Row = m1 in
  let !m2Col = getColumn m2 in
  let (!m1RowLen, !m1ColLen) = (length m1Row, fastLength m1) in
  let (!m2RowLen, !m2ColLen) = (length m2, length m2Col) in
    if isVector m1RowLen m1ColLen && isVector m2RowLen m2ColLen
      then
        let (m1Col, m2Col) = (getColumn m1, getColumn m2) in
        let !left = getSingleLine m1Row m1Col m1RowLen m1ColLen in
        let !right = getSingleLine m2 m2Col m2RowLen m2ColLen in
          vectorMultiply left right
      else
        if not $ canMultiply m1 m2
          then Nothing
          else
            Just $
              let strategy row = rpar $ map (`singleMultiply'` row) m2Col in
              parMap strategy id m1Row
  where
    singleMultiply' :: [Int] -> [Int] -> Int
    singleMultiply' !a !b = F.fold F.sum (zipWith (\ !x !y -> x * y) a b)

    fastLength :: [[Int]] -> Int
    fastLength [] = 0
    fastLength (x:xs) = length x

isVector :: Int -> Int -> Bool
isVector m1r m1c  = m1r == 1 || m1c == 1

getSingleLine :: [[Int]] -> [[Int]] -> Int -> Int -> ([Int], Int)
getSingleLine row col rowL colL =
  if rowL == 1
    then (,) (head row) $!! colL
    else (,) (head col) $!! rowL

vectorMultiply :: ([Int], Int) -> ([Int], Int) -> Maybe [[Int]]
vectorMultiply (a, al) (b, bl) =
  if al /= bl
    then Nothing
    else Just [[singleMultiply a b]]


getRow:: [[a]] -> [[a]]
getRow = id

getColumn :: [[a]] -> [[a]]
getColumn = transpose
  where
  -- take from OldList.hs (core) because i want strict evaluation
  transpose               :: [[a]] -> [[a]]
  transpose []             = []
  transpose ([]   : xss)   = transpose xss
  transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])


singleMultiply :: [Int] -> [Int] -> Int
singleMultiply a b = foldl' (\m (a,b) -> m + a * b) (0 :: Int) (Prelude.zip a b)


canMultiply :: [[a]] -> [[a]] -> Bool
canMultiply a b = (null a && null b) || (Prelude.length (head a) == Prelude.length b)
