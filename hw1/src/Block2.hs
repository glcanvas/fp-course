{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Block2 (
  removeByIndex,
  mergeSort,
  randomIntList
)
where

import System.Random (newStdGen, randomRs)

-- | Function from outside
randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- | Remove element by index if index correct in Just
--  otherwise return Nothing
removeByIndex :: forall a . Int -> [a] -> Maybe a
removeByIndex pos array
  | pos < 0 = Nothing
  | pos >= length array = Nothing
  | otherwise = getItem 0 array
    where
      getItem :: Int -> [a] -> Maybe a
      getItem n (x:xs) =
        if n < pos
        then getItem (n + 1) xs
        else Just x
      getItem _ [] = Nothing

-- | Just merge sort
--
-- >>> [1,7,2,8]
-- [1,2,7,8]
--
-- >>> []
-- []
mergeSort :: forall a . Ord a => [a] -> [a]
mergeSort array = let len = length array in
  case len of
    0 -> array
    1 -> array
    n ->  let middle = div n  2 in
      let first = mergeSort $ firstPath array middle in
        let second = mergeSort $ secondPath array middle 0 in
          mergeArray first second
            where
                firstPath :: [a] -> Int -> [a]
                firstPath _ 0 = []
                firstPath (x:xs) n1 = x : firstPath xs (n1 - 1)
                firstPath [] _ = []
                secondPath :: [a] -> Int -> Int -> [a]
                secondPath [] _ _ = []
                secondPath (x:xs) middle n1 =
                  if n1 < middle
                  then secondPath xs middle (n1 + 1)
                  else x : secondPath xs middle (n1 + 1)
                mergeArray :: [a] -> [a] -> [a]
                mergeArray [] ys = ys
                mergeArray xs [] = xs
                mergeArray (x:xs) (y:ys) =
                  if x < y
                  then x : mergeArray xs (y:ys)
                  else y : mergeArray (x:xs) ys