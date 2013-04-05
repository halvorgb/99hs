data Encoding a = Single a | Multiple Int a
                deriving (Show)


myEncodeDirect :: (Eq a) => [a] -> [Encoding a]

myEncodeDirect [] = []
myEncodeDirect (x:xs) = (myEncodeDirect' xs x 1)


  
myEncodeDirect' :: (Eq a) => [a] -> a -> Int -> [Encoding a]
myEncodeDirect' [] r n
  | n == 1 = [Single r]
  | otherwise = [Multiple n r]
myEncodeDirect' (x:xs) r n
  | x == r = myEncodeDirect' xs r (n+1) 
  | n == 1 = Single r : myEncodeDirect' xs x 1
  | otherwise = Multiple n r : myEncodeDirect' xs x 1
