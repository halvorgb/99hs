import Data.List
import Data.Maybe

solve' :: Int -> Int -> [(Int, Int)]
solve' mn mx
  | mn > mx   = []
  | odd mn    = solve' (mn+1) mx
  | otherwise = solve mn:solve' (mn+1) mx

solve'' :: Int -> Int -> Int -> [(Int, Int)]
solve'' mn mx mxmx
  | mn > mx   = []
  | odd mn    = solve'' (mn+1) mx mxmx
  | ((f > mxmx) && (l > mxmx)) = solf:solve'' (mn+1) mx mxmx
  | otherwise = solve'' (mn+1) mx mxmx
  where
    solf@(f, l) = solve mn
    

solve :: Int -> (Int, Int)
solve n = (i, (n-i))
  where
    primes = generatePrimes n -- NANANANANA BRUTEMAN
    i = fromJust (find (\x -> elem (n-x) primes) primes)

generatePrimes :: Int -> [Int]

generatePrimes 2 = [2] 
generatePrimes n
  | even n = memoizedPrimes
  | not (any (\x -> mod n x == 0) (takeWhile (<n) memoizedPrimes)) =  n:memoizedPrimes
  | otherwise = memoizedPrimes

    where 
      memoizedPrimes = generatePrimes (n-1)