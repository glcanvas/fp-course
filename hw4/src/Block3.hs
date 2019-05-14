{-# LANGUAGE Strict #-}

module Block3 (
    gauss
  , verifySolution
) where

import Control.Parallel
import Control.Parallel.Strategies

-- | define array of bool as Vector for complicated
type Vector = [Bool]
-- | define array of Vector as Matrix for complicated
type Matrix = [Vector]

-- | function that evaluate gauss algorithm and return Just _ if solve singe
-- otherwise Nothing
gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss matrix vector =
  let addYLabel = zipWith (\x y -> x ++ [y]) matrix vector in do
    value <- triangular addYLabel
    pure $  solveMatrix (map reverse value)
  where
  solveMatrix :: Matrix -> Vector
  solveMatrix = resolveAllColumns . reverse

  resolveAllColumns :: Matrix -> Vector
  resolveAllColumns [] = []
  resolveAllColumns matrix =
    let (mH : mT) = matrix in
    let currentItem = head mH && last mH in
    let flat = parMap (\(r' : (r'' : r''')) -> rpar $ runEval (rdeepseq $ xor r' (currentItem && r'') : r''')) id mT in
    currentItem : resolveAllColumns flat

  setZeros :: Matrix -> Vector -> Matrix
  setZeros matrix v = parMap (\x -> rpar $ runEval (rdeepseq $ x `inner` v)) id matrix
    where
      inner :: Vector -> Vector -> Vector
      inner [] _ = []
      inner _ [] = []
      inner (x:xs) (v:vs)
        | not x = xs
        | otherwise = zipWith xor xs vs

  notZero :: Matrix -> Maybe Matrix
  notZero [] = Nothing
  notZero (x:xs)
    | head x = Just $ x : xs
    | otherwise = do
        v <- notZero xs
        pure $ v ++ [x]

  triangular :: Matrix -> Maybe Matrix
  triangular [] = Just []
  triangular m = do
        (matrixHead : matrixTail) <- notZero m
        let newMatrix = setZeros matrixTail matrixHead
        v <- triangular newMatrix
        pure $ matrixHead : v

-- | xor function from wiki
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

-- | function verify that matrix x vector equal other vector
verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution a x y = inner a x == y
  where
    inner rows column = foldl (\b a -> multiply' column a : b) [] rows
    multiply' row1 row2 = and (zipWith xor row1 row2)