module SimpleMult (
  multiply
  ) where

import Control.Monad.ST
import Data.STRef
import Data.List

-- | all function is lazy and whorse than functions from Block2
-- need for show performance
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
  where
  isVector :: Int -> Int -> Bool
  isVector m1r m1c  = m1r == 1 || m1c == 1

  getSingleLine :: [[Int]] -> [[Int]] -> Int -> Int -> ([Int], Int)
  getSingleLine row col rowL colL =
    if rowL == 1
      then (,) (head row) colL
      else (,) (head col) rowL

  vectorMultiply :: ([Int], Int) -> ([Int], Int) -> Maybe [[Int]]
  vectorMultiply (a, al) (b, bl) =
    if al /= bl
      then Nothing
      else Just [[singleMultiply a b]]

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
  singleMultiply a b = foldl (\m (a,b) -> m + a * b) (0 :: Int) (Prelude.zip a b)


  canMultiply :: [[a]] -> [[a]] -> Bool
  canMultiply a b = (null a && null b) || (Prelude.length (head a) == Prelude.length b)
