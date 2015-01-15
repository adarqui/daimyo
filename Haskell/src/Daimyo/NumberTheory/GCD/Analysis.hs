module Daimyo.NumberTheory.GCD.Analysis (
    maxSteps,
    avgSteps,
    overallComplexity
) where

{-
    Lame, Dixon, Heilbronn
-}

maxSteps n = ceiling (log (n*(sqrt 5)) / log ((1+(sqrt 5))/2)) - 2

avgSteps n = ((12 * log 2) / (pi ** 2)) * log n

-- bit operations
overallComplexity n = (log n)**2
