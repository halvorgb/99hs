data Encoding a = Single a | Multiple Int a
                deriving (Show)



myEncode :: (Eq a) => [a] -> [Encoding a]

myEncode [] = []
myEncode x = myRunLengthEncoding (myPack x)


myRunLengthEncoding :: (Eq a) => [[a]] -> [Encoding a]

myRunLengthEncoding [] = []
myRunLengthEncoding (x:xs)
  | lx == 1 = Single hx:myRunLengthEncoding xs
  | otherwise = Multiple lx hx:myRunLengthEncoding xs
  where lx = length x
        hx = head x


myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = myPack' xs [x]

myPack' :: (Eq a) => [a] -> [a] -> [[a]]
myPack' [] r = [r]
myPack' (x:xs) r
  | x == (head r) = myPack' xs (x:r)
  | otherwise = (r:myPack' xs [x])

