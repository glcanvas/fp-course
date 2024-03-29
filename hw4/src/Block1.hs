{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict#-}

module Block1 (
    multiply
) where

import Data.List
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

    getColumn :: [[a]] -> [[a]]
    getColumn = transpose

    canMultiply :: [[a]] -> [[a]] -> Bool
    canMultiply a b = Prelude.length (head a) == Prelude.length b
