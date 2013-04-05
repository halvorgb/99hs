myDuplicate :: [a] -> Int -> [a]

myDuplicate [] _ = []
myDuplicate (x:xs) n = replicate n x ++ myDuplicate xs n