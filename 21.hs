myInsertAt :: Char -> String -> Int -> String

myInsertAt y [] _ = [y]
myInsertAt y (x:xs) n
  | n > 1 = x: myInsertAt y xs (n-1)
  | otherwise = y : x : xs