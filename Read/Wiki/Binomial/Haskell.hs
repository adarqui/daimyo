module Wiki.Binomial.Haskell (
) where

{-  sources:
    http://en.wikipedia.org/wiki/Binomial_distribution
-}

import Data.List
import Wiki.E.Haskell

probMass k n p = (n `choose` k) * (p**k) * ((1-p)**(n-k))

nk n k =  (fact n) / (fact k * fact (n-k))

n `choose` k
    | k < 0 = 0
    | k > n = 0
    | otherwise = nk n k

{-
Suppose a biased coin comes up heads with probability 0.3 when tossed. What is the probability of achieving 0, 1,..., 6 heads after six tosses?
-}
t_probMass = map (\i -> let (n, k) = (6-i, i) in probMass k n 0.3) [0..6]
