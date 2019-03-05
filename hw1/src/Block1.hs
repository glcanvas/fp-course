module Block1 (
  order3,
  smartReplicate,
  contains,
  stringSum
) where

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) =  let first = min (min x y) z in
  let second = max (min x y) (max (min y z) (min x z)) in
    let third = max (max x y) z in
      (first, second, third)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains a = filter (elem a)

stringSum :: String -> Int
stringSum str = foldl (\b a -> (+) b (read a :: Int)) 0 (words str)
