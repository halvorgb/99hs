-- ez way
mySplit :: [a] -> Int -> ([a],  [a])

mySplit [] _ = ([], [])
mySplit xs n = (take n xs, drop n xs)

-- not as ez way, not allowed to use take nor drop
mySplit' :: [a] -> Int -> ([a], [a])

mySplit' [] _ = ([], [])
mySplit' (x:xs) n
  | n > 0 = (x : ys, zs)
  | otherwise = ([], x:xs)
  where
    (ys, zs) = mySplit xs (n-1)

