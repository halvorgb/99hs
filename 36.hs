main = interact solve

solve :: String -> String
solve str  = show $ totient ( factorShortForm ( factor $ read str) )


totient :: [(Int, Int)] -> Int

totient [] = 1
totient ((p, m):fs) = (p-1)*p^(m-1) * totient fs




factorShortForm :: [Int] -> [(Int, Int)]
factorShortForm [] = []
factorShortForm l@(x:xs) = (x, xl):factorShortForm (drop xl l)
  where
    xl = length (takeWhile (x==) l)
    
    

factor :: Int -> [Int]
factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime