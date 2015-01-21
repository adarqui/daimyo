module Daimyo.NumberTheory.Consecutive (
    cis,
    csBase
) where

import Data.List

-- consecutive integer sequence: 1 12 123 1234..
-- cis = tail $ scanl (\b a -> b++[a]) [] [1..]
-- I need to do this a more elegant way multiplying by position but it gets tricky and i don't have time right now. TODO FIXME
cis = undefined

-- csBase: consecutive sequence of base b
csBase b = undefined
