-- finn lengden, finn midten.
-- sammenlign start -> midt med end -> midt

isPalindrome :: (Eq a) => [a] -> Bool


isPalindrome [] = False
isPalindrome l = ((myPick l $ quot (myLength l) 2) == (myPick (myReverse l) $ quot (myLength l) 2))





myPick :: [a] -> Int -> [a]
myPick [] _     = []
myPick _ 0      = [] 
myPick (x:xs) i = x:myPick xs (i-1) 


myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

myLength :: [a] -> Int
myLength [_] = 0
myLength (x:xs) = 1 + length xs