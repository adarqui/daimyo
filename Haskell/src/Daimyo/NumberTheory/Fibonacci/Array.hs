module Daimyo.NumberTheory.Fibonacci.Array (
  fibArray
) where

import           Daimyo.NumberTheory.Fibonacci
import           GHC.Arr

-- | fibArray
--
-- >>> fibArray 5 :: Array Int Integer
-- array (1,5) [(1,0),(2,1),(3,1),(4,2),(5,3)]
--
fibArray :: Integral a => Int -> Array Int a
fibArray n = array (1,n) $ zip [1..n] fibs
  where
    fibs = take n $ fibonacciNumbers
