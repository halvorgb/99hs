solve :: Int -> Int -> [Int]

solve mn mx = filter (>mn) $ generatePrimes mx

generatePrimes :: Int -> [Int]

generatePrimes 1 = [2] -- a hack to skip some recursions (half of em)
generatePrimes n
  | even n = memoizedPrimes
  | not (any (\x -> mod n x == 0) (takeWhile (<n) memoizedPrimes)) =  n:memoizedPrimes
  | otherwise = memoizedPrimes

    where 
      memoizedPrimes = generatePrimes (n-1)