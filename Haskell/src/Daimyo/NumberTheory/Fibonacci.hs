module Daimyo.NumberTheory.Fibonacci (
  fibonacci,
  fibonacciNumbers,
  fibonacciGCD
) where

-- | fibonacciNumbers
--
-- >>> take 10 $ fibonacciNumbers :: [Int]
-- [0,1,1,2,3,5,8,13,21,34]
--
fibonacciNumbers :: Integral a => [a]
fibonacciNumbers = 0 : 1 : 1 : go 1 1
  where
    go p c = (p + c) : go c (p+c)

-- | fibonacci
--
-- >>> fibonacci 10 :: Int
-- 55
--
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- | fibonacciGCD
--
-- >>> fibonacciGCD 10 20 :: Int
-- 55
--
fibonacciGCD :: Integral a => a -> a -> a
fibonacciGCD m n = fibonacci (gcd m n )
