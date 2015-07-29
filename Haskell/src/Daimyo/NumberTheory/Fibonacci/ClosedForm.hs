module Daimyo.NumberTheory.Fibonacci.ClosedForm (
  fib,
  fib',
  sqrtIntegral
) where

-- | fib
--
-- source: http://mathworld.wolfram.com/FibonacciNumber.html
--
-- ((1 + sqrt 5)^n - (1 - sqrt 5)^n) / (2^n * sqrt 5)
--
-- >>> fib 10 :: Int
-- 55
--
fib :: Integral a => a -> a
fib n = truncate $ ((1 + sqrt(5))**n' - (1 - sqrt(5))**n') / ((2**n') * sqrt(5))
  where n' = fromIntegral n :: Double

-- | fib'
--
-- borked.
--
fib' :: Integral a => a -> a
fib' n = ((1 + sqrtIntegral(5))^n - (1 - sqrtIntegral(5))^n) `div` ((2^n) * sqrtIntegral(5))

-- | sqrtIntegral
--
-- >>> sqrtIntegral 28
-- 5
--
sqrtIntegral :: Integral a => a -> a
sqrtIntegral n = truncate $ sqrt (fromIntegral n :: Double)
