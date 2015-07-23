module Daimyo.Algorithm.Growth.Detective (
  detect,
  detectN,
  detectCompare,
  detectTests
) where

import Daimyo.Algorithm.Growth
import Daimyo.NumberTheory.Prime
import Daimyo.NumberTheory.Factorial
import Daimyo.NumberTheory.Fibonacci
import Data.List

-- | detectN
--
-- experimental functions for finding the O(...) growth of a function
--
-- >>>
--
detectN :: (Double -> Double) -> Double -> (String, Double)
detectN f n = detect f [1..n]

-- | detect
--
--
detect :: (Double -> Double) -> [Double] -> (String, Double)
detect f interval = minimumBy (\(name1,sum1) (name2,sum2) -> compare sum1 sum2) sums
  where
    comparisons = map (detectCompare f interval) growthFunctions
    sums        = map (\(name,values) -> (name, sum values)) comparisons
    z           = sum $ map f interval

-- | detectCompare
--
-- compare a function, up to n values, against g (a growth function structure)
--
-- >>>
--
detectCompare :: (Double -> Double) -> [Double] -> Growth -> (String, [Double])
detectCompare f interval g = (_name g, testGrowthFunctionResults results g_results)
  where
    results = gaps $ map f interval
    g_results = (_growthFn g) interval

-- | detectTests
--
detectTests :: [(String, Double)]
detectTests =
  [
    detect (\x -> 3*x+100) interval,
    detect (\x -> 3*x) interval,
    detect (\x -> 3*(x*x)+100) interval,
    detect (\x -> 3*2**x) interval,
    detect (\x -> 3*(fac x)+100) interval,
    detect (\x -> 3*(fromIntegral (fibonacciNumbers !! (truncate x)) :: Double)+100) interval,
    detect (\x -> 3*(log x)+100) interval,
    detect (\x -> 3*(log (log x))+100) interval,
    detect (\x -> 10+sin x) interval,
    detect (\x -> 10+cos x) interval,
    detect (\x -> 10+tan x) interval,
    detect (\x -> 3+(fromIntegral (primes !! (truncate x)) :: Double)+100) interval
  ]
  where
    interval = [10..100]
