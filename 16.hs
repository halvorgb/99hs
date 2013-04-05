myDropEveryNth :: [a] -> Int -> [a]

myDropEveryNth [] _ = []
myDropEveryNth xs n = myDropHelper xs n n

myDropHelper :: [a] -> Int -> Int -> [a]

myDropHelper [] _ _ = []
myDropHelper (x:xs) n i
  | i == 1 = myDropHelper xs n n
  | otherwise = x:myDropHelper xs n (i-1)