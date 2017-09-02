module Daimyo.Delrik (
    euler'p2,
    isPrime,
    primes
) where

import Data.List

-- http://owp.ghost.io/project-euler-1/
--
--
isPrime :: (Enum a, Ord a, Integral a) => a -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n = null [x | x <- possibles, n `mod` x == 0]
    where
        possibles = takeWhile (\x -> x*x <= n) [2..n]
primes = 1 : 2 : filter isPrime [3..]

euler'p2 :: (Floating a, Integral a) => a -> [(a,a)]
euler'p2 x = takeWhile isPrime $ zip [x,(x-1)..hf] [1..x]
    where
        hf = sqrt x
        op y= (x `mod` y)
        isPrime (x',y') = case (op y') of 
            0 -> False;
            _ -> True
