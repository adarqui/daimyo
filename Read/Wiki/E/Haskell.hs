module Wiki.E.Haskell (
    facAndE,
    fact,
    e
) where

{- source: http://en.wikipedia.org/wiki/E_(mathematical_constant) -}

import Data.List

fact' 0 = 1
fact' n = n * fact' (n-1)

e' 0 = 1
e' n = (1/fact' n) + e' (n-1)

facAndE n = foldl' (\(fac,e') i -> let fac' = (i*fac) in (fac', e' + (1/fac'))) (1,1) [1..n]

fact = fst . facAndE
e = snd . facAndE
