myTotient :: Int -> Int

myTotient x = length [t | t<-[1..(x-1)], myCoPrime x t]



myCoPrime :: Int -> Int -> Bool

myCoPrime a b = myGCD a b == 1


myGCD :: Int -> Int -> Int

myGCD a b
  | b == 0 = a
  | a == 0 = b
  | a > b =  myGCD (a-b) b
  | otherwise = myGCD a (b-a)