import Control.Monad

main :: IO ()
main = do
 _T <- readLn :: IO Int
 nums <- mapM (\_ -> readLn :: IO Int) [1.._T]
 mapM_ (\n -> putStrLn $ show $ solve n) nums

solve n = length $ takeWhile (<= w) primes
 where
  w = wall n

-- bricks: 4x1, 1x4
wall 0 = 1
wall 1 = 1
wall 2 = 1
wall 3 = 1
wall n = wall (n-1) + wall (n-4)

isPrime n = n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes

primes = 2 : filter isPrime [3,5..]
