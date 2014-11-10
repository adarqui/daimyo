{-
  source: https://www.hackerrank.com/challenges/reverse-factorization

  input:
   line 1: N (final value to reach) K (size of set A)
   line 2: set A (Ints)

  output:
   print the steps to reach N if it exists
   otherwise print -1

  Constraints 
   1 ≤ N ≤ 10^9
   1 ≤ K ≤ 20 
   2 ≤ ai ≤ 20, where i ∈ [1..K] 
   ai ≠ aj, where 1 <= i, j <= K AND i ≠ j
-}

import Data.List

main :: IO ()
main = do
 (nk:seta:[]) <- sequence [getLine, getLine]
 let (_N, _K) = getNumTup nk :: (Int, Int)
 let _A = getNums seta :: [Int]
 case (isPrime _N) of
  False -> putStrLn $ show $ reverse'fac _N _K _A
  True -> putStrLn "-1"

--reverse'fac n k a = minimum $ subseqs $ filter (\fac -> isFactor fac n) a
reverse'fac n k a = subseqs a
-- sortBy (\x y -> compare (length x) (length y)) $ subseqs [2,3,4]

--t1 = reverse'fac 12 3 $ filter (\n -> isFactor n 12) [2,3,4]
 
getNumTup s = let w = words s in (read (w !! 0), read (w !! 1))
getNums s = map read . words $ s 
isPrime n = n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
primes = 2 : filter isPrime [3,5..]
isFactor fac n = n `rem` fac == 0
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]
