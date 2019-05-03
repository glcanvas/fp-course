{-# LANGUAGE ScopedTypeVariables #-}

module Block1 where

import Control.Monad.ST
import Data.STRef
import Control.Monad

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply = undefined


getColumn :: [[a]] -> [[a]]
getColumn = id

getRow :: [[a]] -> [[a]]
getRow = undefined

array = [[1,2,3], [4,5,6]]


transpose :: forall a . [[a]] -> [[a]]
transpose [] = []
transpose (x:xs) =
  let stubArray = replicate (length x) [] in
    map reverse $ innerTrans (x:xs) stubArray
  where
    innerTrans :: [[a]] -> [[a]] -> [[a]]
    innerTrans array stub = foldl inner stub array

    inner :: [[a]] -> [a] -> [[a]]
    inner (b:bs) (a:as) = (a : b) : inner bs as
    inner [] [] = []


