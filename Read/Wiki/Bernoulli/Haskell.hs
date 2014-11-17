module Wiki.Bernoulli.Haskell (
) where

{-  sources:
    http://en.wikipedia.org/wiki/Bernoulli_trial
    http://en.wikipedia.org/wiki/Bernoulli_process
    http://en.wikipedia.org/wiki/Bernoulli_distribution
-}

data Outcome = S | F deriving (Show, Read, Eq)
