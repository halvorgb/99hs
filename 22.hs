myRange :: Int -> Int -> [Int]

myRange x y
  | x < y =  x: myRange (x+1) y
  | otherwise = [y]