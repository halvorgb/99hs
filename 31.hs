myIsPrime :: Int -> Bool

-- build a list of all primes of size <= sqrt n.
-- check n vs. that list
myIsPrime n
  | ( filter (\x -> (n `mod` x) == 0) primes )== [] = True
  | otherwise = False    
    where
      primes = [t | t <- [2,3..(n-1)], odd t,( filter (\x -> (t `mod` x) == 0) [2..(t-1)] )== []]
      
-- fuck it naive solution, the other one was shit anyways...
myIsPrime' :: Int -> Bool
myIsPrime' n = [t | t <- [2..(n-1)], n `mod` t == 0] == []
      


