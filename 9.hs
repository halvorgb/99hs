myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = myPack' xs [x]
                

myPack' :: (Eq a) => [a] -> [a] -> [[a]]
myPack' [] r = [r]
myPack' (x:xs) r
  | x == (head r) = myPack' xs (x:r)
  | otherwise = (r:myPack' xs [x])


