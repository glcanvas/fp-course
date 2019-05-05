{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Block1 where

import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.List
import Control.Loop

import Data.Array.ST
import Data.Array.Base

import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies

kek = take 1_000_000 (repeat 4)

lol = runEval $ do
  a <- rpar kek
  b <- rpar kek
  pure (a, b)

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply m1 m2 =
  let (m1Row, m1Col) = (getRow m1, getColumn m1) in
  let (m2Row, m2Col) = (getRow m2, getColumn m2) in
  let (m1RowLen, m1ColLen) = (Prelude.length m1Row, Prelude.length m1Col) in
   let (m2RowLen, m2ColLen) = (Prelude.length m2Row, Prelude.length m2Col) in
    if isVector m1RowLen m1ColLen && isVector m2RowLen m2ColLen
      then
        let left = getSingleLine m1Row m1Col m1RowLen m1ColLen in
        let right = getSingleLine m2Row m2Col m2RowLen m2ColLen in
          vectorMultiply left right
      else
        if not $ canMultiply m1 m2
          then Nothing
          else Just $
            runST $ do
              emptyArray <- newSTRef []
              Prelude.mapM_
                (\col ->
                  Prelude.mapM_
                    (\row -> do
                      let value = singleMultiply col row
                      v <- readSTRef emptyArray
                      writeSTRef emptyArray (value : v))
                      m2Col)
                    m1Row
              v <- readSTRef emptyArray
              pure $ fromList' m2ColLen (Prelude.reverse v)


isVector :: Int -> Int -> Bool
isVector m1r m1c  = m1r == 1 || m1c == 1

getSingleLine :: [[Int]] -> [[Int]] -> Int -> Int -> ([Int], Int)
getSingleLine row col rowL colL = if rowL == 1 then (head row, colL) else (head col, rowL)

vectorMultiply :: ([Int], Int) -> ([Int], Int) -> Maybe [[Int]]
vectorMultiply (a, al) (b, bl) =
  if al /= bl
    then Nothing
    else pure $ pure $ pure $ singleMultiply a b


fromList' :: Int -> [a] -> [[a]]
fromList' n = inner
  where
    inner :: [a] -> [[a]]
    inner [] = []
    inner x = Prelude.take n x : inner (Prelude.drop n x)

getRow:: [[a]] -> [[a]]
getRow = id

getColumn :: [[a]] -> [[a]]
getColumn = transpose

singleMultiply :: [Int] -> [Int] -> Int
singleMultiply a b = foldl' (\m (a,b) -> m + a * b) (0 :: Int) (Prelude.zip a b)


-- | function that generate matrix with a columns & b rows
matrixGenerate :: Int -> Int -> [[Int]]
matrixGenerate a b
  | a  > 0 && b > 0 = runST $ do
    emptyArray <- newSTRef []
    numLoop 0 (a - 1)
      (\i -> do
         let generate = [a * i + 1 .. a * i + b]
         v <- readSTRef emptyArray
         writeSTRef emptyArray (generate : v))
    readSTRef emptyArray
  | otherwise = error "asd"

c x y  = do
  print $ multiply x y
  putStrLn (show x <> " * " <> show y)
  putStrLn "============================"
  putStrLn ""

printInfo x =
  "row = " <> show (getRow x) <> " row length = " <> show (Prelude.length (getRow x)) <> " \n " <> "col = " <>
  show (getColumn x) <>
  " col length = " <>
  show (Prelude.length (getColumn x)) <>
  " \n"

canMultiply :: [[a]] -> [[a]] -> Bool
canMultiply a b = (null a && null b) || (Prelude.length (head a) == Prelude.length b)

c' x y = canMultiply x y

tests a =
  forM_
    [1 .. a]
    (\i -> forM_ [1 .. a] (\i' -> forM_ [1 .. a] (\j -> forM_ [1 .. a] (c (matrixGenerate i i') . matrixGenerate j))))


matrixs =
  Prelude.map (\x -> (x, matrixGenerate x x, matrixGenerate x x)) [5, 10, 50, 100, 250]

matrixMult =
  Prelude.map (\(a, l, r) -> bench (show a) $ nf (multiply l) r) matrixs

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