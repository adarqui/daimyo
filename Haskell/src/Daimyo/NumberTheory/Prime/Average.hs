module Daimyo.NumberTheory.Prime.Average (
    average
) where

import Daimyo.NumberTheory.Prime

data Average a = Average {
    k :: Int,
    pk :: a,
    sumpk :: a,
    avgpk :: Int
} deriving (Show)

average ps = Average {
    k = length ps,
    pk = last ps,
    sumpk = sum ps,
    avgpk = (fromIntegral (sum ps)) `div` (length ps)
}

t_average'1 = average (take 1 primes)
t_average'23 = average (take 23 primes)
