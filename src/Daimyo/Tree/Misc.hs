module Daimyo.Tree.Misc (
    numTreesFromList
) where

import Data.List

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

numTreesFromList l = ((fac (2*n))`div`((fac (n+1))*(fac n))) `mod` modp
    where
        nubed = nub l
        n = toInteger $ length nubed

modp = ((10^8)+7)
