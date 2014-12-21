module Daimyo.ConcreteMathematics.Sums (
    sigma,
    (∑)
) where

sigma k n ak = sum $ map ak [k..n]

(∑) :: Int -> Int -> (Int -> Int) -> Int
(∑) k n ak = sigma k n ak

t_sigma'1 = sigma 0 49 (\k -> (2*k+1)^2)
