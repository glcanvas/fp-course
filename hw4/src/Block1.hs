{-# LANGUAGE ScopedTypeVariables #-}

module Block1 where

import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.List
import Control.Loop

import Criterion.Main

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply matrix1 matrix2 =
  let cols1 = getColumn matrix1
   in let rows2 = getRow matrix2
       in if length cols1 /= length rows2
            then Nothing
            else Just $
                 runST $ do
                   emptyArray <- newSTRef []
                   mapM_
                     (\col ->
                        mapM_
                          (\row -> do
                             let value = singleMultiply col row
                             v <- readSTRef emptyArray
                             writeSTRef emptyArray (value : v))
                          rows2)
                     cols1
                   v <- readSTRef emptyArray
                   pure $ fromList (length rows2) (reverse v)

fromList :: Int -> [a] -> [[a]]
fromList n = inner
  where
    inner :: [a] -> [[a]]
    inner [] = []
    inner x = take n x : inner (drop n x)

getColumn :: [[a]] -> [[a]]
getColumn = id

getRow :: [[a]] -> [[a]]
getRow = transpose

singleMultiply :: [Int] -> [Int] -> Int
singleMultiply a b = foldl' (\m (a,b) -> m + a * b) (0 :: Int) (zip a b)


-- | function that generate matrix with a columns & b rows
matrixGenerate :: Int -> Int -> Maybe [[Int]]
matrixGenerate a b
  | a  > 0 && b > 0 = Just $ runST $ do
    emptyArray <- newSTRef []
    numLoop 0 (a - 1)
      (\i -> do
         let generate = [a * i .. a * i + b - 1]
         v <- readSTRef emptyArray
         writeSTRef emptyArray (generate : v))
    readSTRef emptyArray
  | otherwise = Nothing

matrixs =
  map (\x -> (x, matrixGenerate x x, matrixGenerate x x)) [5, 10, 50, 100, 250]

matrixMult =
  map (\(a, Just l, Just r) -> bench (show a) $ nf (multiply l) r) matrixs

main' = defaultMain [ {-

  bgroup "matrix generate" [
    bench "5" $ nf (matrixGenerate 5) 5,
    bench "10" $ nf (matrixGenerate 10) 10,
    bench "50" $ nf (matrixGenerate 50) 50,
    bench "100" $ nf (matrixGenerate 100) 100,
    bench "500" $ nf (matrixGenerate 500) 500,
    bench "1000" $ nf (matrixGenerate 1000) 1000
    ]
-}
    bgroup "matrix mull" matrixMult
    ]