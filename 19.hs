myRotateNLeft :: [a] -> Int -> [a]

myRotateNLeft l@(x:xs) n
  | n > 0 = myRotateNLeft (xs++[x]) (n-1)       -- left rotation
  | n < 0 = myRotateNLeft (last l:init l) (n+1) -- right rotation
  | otherwise = l