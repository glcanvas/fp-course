{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict#-}

module Block1 (
    multiply
) where

import Control.Monad.ST
import Data.STRef
import Data.List

import Control.Parallel
import Control.Parallel.Strategies
import qualified Control.Foldl as F

-- | function that multiply two matrix and return new matrix or Nothing if their size not comarable
multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply m1 m2 =
  let rows = m1 in
  let columns = getColumn m2 in
  if not $ canMultiply m1 m2
    then Nothing
    else
      Just $
        let inner row = runEval (rdeepseq $ map (`singleMultiply'` row) columns) in
        let strategy row = rpar $ inner row in
          parMap strategy id rows
  where
    singleMultiply' :: [Int] -> [Int] -> Int
    singleMultiply' !a !b = F.fold F.sum (zipWith (\ !x !y -> x * y) a b)

    fastLength :: [[Int]] -> Int
    fastLength [] = 0
    fastLength (x:xs) = length x

    getRow:: [[a]] -> [[a]]
    getRow = id

    getColumn :: [[a]] -> [[a]]
    getColumn = transpose

    singleMultiply :: [Int] -> [Int] -> Int
    singleMultiply a b = foldl' (\m (a,b) -> m + a * b) (0 :: Int) (Prelude.zip a b)


    canMultiply :: [[a]] -> [[a]] -> Bool
    canMultiply a b = Prelude.length (head a) == Prelude.length b
