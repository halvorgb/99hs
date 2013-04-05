import Data.List
import Data.Maybe

solve :: Int -> (Int, Int)

solve n = (i, (n-i))
  where
    primes = generatePrimes n
    i = fromJust (find (\x -> elem (n-x) primes) primes)

generatePrimes :: Int -> [Int]

generatePrimes 2 = [2] 
generatePrimes n
  | even n = memoizedPrimes
  | not (any (\x -> mod n x == 0) (takeWhile (<n) memoizedPrimes)) =  n:memoizedPrimes
  | otherwise = memoizedPrimes

    where 
      memoizedPrimes = generatePrimes (n-1)