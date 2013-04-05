mySlice :: [a] -> Int -> Int -> [a]

mySlice [] _ _ = []
mySlice (x:xs) s e
  | s > 1 = mySlice xs (s-1) (e-1)
  | e > 0 = x: mySlice xs s (e-1)
  | otherwise = []