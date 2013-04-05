myRemoveAt :: Int -> [a] -> [a]

myRemoveAt _ [] = []
myRemoveAt n (x:xs)
  | n > 1 = x:myRemoveAt (n-1) xs  --1 indexed...
  | otherwise = xs