myPrimeFactors :: Int -> [Int]

myPrimeFactors n = [x | x <-[2..(n-1)], mod n x == 0, myIsPrime' x]
    
    
myIsPrime' :: Int -> Bool
myIsPrime' n = [t | t <- [2..(n-1)], n `mod` t == 0] == []
                                 
                                 
                                 