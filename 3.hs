elementAt :: [a] -> Integer -> a

elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs $ i - 1
