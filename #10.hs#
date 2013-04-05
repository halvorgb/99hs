myEncode :: (Eq a) => [a] -> [(Int, a)]

myEncode [] = []
myEncode x = myRunLengthEncoding (myPack x)


myRunLengthEncoding :: [[a]] -> [(Int, a)]

myRunLengthEncoding [] = []
myRunLengthEncoding (x:xs) = (length x, head x) : myRunLengthEncoding xs


myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = myPack' xs [x]

myPack' :: (Eq a) => [a] -> [a] -> [[a]]
myPack' [] r = [r]
myPack' (x:xs) r
  | x == (head r) = myPack' xs (x:r)
  | otherwise = (r:myPack' xs [x])


