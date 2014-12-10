module Daimyo.Lib.Wave.ProbabilityTheory (
) where

import GHC.Float
import Data.Set
import qualified Data.List as L

data Coin = H | T deriving (Show, Eq)

pnats n = fromList [1..n]

data ProbabilitySet a = PSet {
    name :: String,
    possibleCases :: Set a,
    favorableCases :: Set a,
    possibleOutcomes :: Int
} deriving (Show)

setSize s = fromIntegral (size s) :: Float

subseqs :: [a] -> [[a]]
subseqs l = [x | i <- L.inits l, x <- L.tails i, not $ L.null x ]

subsequences = L.filter (not . L.null) . L.subsequences

probabilityOf :: ProbabilitySet a -> Float
probabilityOf p = (setSize $ favorableCases p) / (setSize $ possibleCases p)

probabilityOfSets :: [ProbabilitySet a] -> (String, Float)
probabilityOfSets ps =
    let
        names = concat $ L.map name ps
        omegas = product $ L.map (setSize . possibleCases) ps
        events = product $ L.map (setSize . favorableCases) ps
    in
        (names, events / omegas) 

probabilitiesOfSets :: [ProbabilitySet a] -> [(String, Float)]
probabilitiesOfSets ps =
    let
        combos = subsequences ps
    in
        L.map probabilityOfSets combos

{-
    builds a probability set according to the probability measure axioms

(i) For every set A ⊂ Ω, the value of the function is a nonnegative
number: P(A) ≥ 0.
(ii) For any two disjoint sets A and B, the value of the function for
their union A + B is equal to the sum of its value for A and its
value for B:
P(A + B) = P(A) + P(B) provided AB = ∅.
(iii) The value of the function for Ω (as a subset) is equal to 1:
P(Ω) = 1.

-}

psetBuild name p f =
    let
        pset = PSet name p f 0 --(float2Int (setSize f)^(float2Int (setSize p)))
    in
        pset

t_psetBuild =
    let
        _A = psetBuild "A" (pnats 2) (pnats 1)
        _B = psetBuild "B" (pnats 6) (pnats 2)
        _C = psetBuild "C" (pnats 52) (pnats 13)
    in
        (_A, _B, _C)

{-
    Example 1.

    A = rotten apples = {a1...a28}
    B = unripe apples = {u1..u47}
    A and B are disjoint

    |A| = size of rotten apples
    |B| = size of unripe apples
    Ω = size of sample space = 550

    P = proportion

    P(A) = |A|/|Ω| = 28/550
    P(B) = |B|/|Ω| = 47/550
-}

ex1 =
    let
        (_Ω) = 550
        _A = 28
        _PA = _A / _Ω
        _B = 47
        _PB = _B / _Ω
        _PAB = _PA + _PB
    in
    _PAB


{-
    Example 3.

    Ω = {1, 2, 3, 4, 5, 6}
    P({k}) = 1/6, k = 1,2,3,4,5,6.
-}



{-
    Example 7: Repeated Independent Trials

    A = coin falls heads
    B = die shows number 5 or 6
    C = card drawn is a spade

    P(A) = 1/2
    P(B) = 2/6 = 1/3
    P(C) = 13/52 = 1/4
-}

ex6_independent_trials =
    let
        _A = psetBuild "A" (pnats 2) (pnats 1)
        _B = psetBuild "B" (pnats 6) (pnats 2)
        _C = psetBuild "C" (pnats 52) (pnats 13)
    in
        (probabilityOfSets [_A, _B, _C], probabilitiesOfSets [_A, _B, _C])


{-
    Example 8: Repeated Trials

    Coin is tossed repeatedly n times.
    Join outcome may be recorded as a sequence of H's and T's

    H = 1
    T = 0

    2 outcomes for each trial
    2^n possible joint outcomes

    coin not assumed to be fair.
-}


