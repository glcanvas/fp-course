module Block1 (
  order3,
  smartReplicate,
  contains,
  stringSum
) where

-- | Return ordered cortage
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) =  let first = min (min x y) z in
  let second = max (min x y) (max (min y z) (min x z)) in
    let third = max (max x y) z in
      (first, second, third)

-- | Replace each number with array
-- with length equal number of this number
-- concatenate in large array all this little arrays
smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

-- | Return list of lists in which exist element a
contains :: Eq a => a -> [[a]] -> [[a]]
contains a = filter (elem a)

-- | Parse string and get sum of all elements
-- raise error in bad string
stringSum :: String -> Int
stringSum str = foldl (\b a -> (+) b (read a :: Int)) 0 (words str)
