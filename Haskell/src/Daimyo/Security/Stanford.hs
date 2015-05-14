module Daimyo.Security.Stanford (
    Style (..),
    Crack (..),
    crack,
    space
) where

{-
    source: https://raw.githubusercontent.com/thialfihar/xkcd-password-generator/master/wordlist.txt
-}

import Daimyo.Combinatorics.Combination
import Daimyo.Combinatorics.Permutation

data Style = Repetition | Unique deriving (Show, Read)

data Crack = Crack {
    style :: Style,
    totalWords :: Integer, 
    totalWordCombinations :: Integer,
    crackSpace :: Integer
{-
    cracksPerSecond :: Double,
    cracksPerMinute :: Double,
    cracksPerHour :: Double,
    cracksPerDay :: Double,
    cracksPerWeek :: Double,
    cracksPerMonth :: Double,
    cracksPerYear :: Double
-}
} deriving (Show, Read)

{-
    crack total_words total_word_combinations cracks_per_second
-}

crack style' tw twc cps =
    let
        s = space style' tw twc
        s' = fromIntegral s :: Double
        s'' = s' / cps
    in
        Crack {
            style = style',
            totalWords = tw,
            totalWordCombinations = twc,
            crackSpace = s
{-
            cracksPerSecond = s'',
            cracksPerMinute = s' / 60,
            cracksPerHour = s'' / 60 / 60,
            cracksPerDay = s'' / 60 / 60 / 24,
            cracksPerWeek = s'' / 60 / 60 / 7,
            cracksPerMonth = s'' / 60 / 60 / 24 / 30,
            cracksPerYear = s'' / 60 / 60 / 24 / 30 / 365
-}
        }

space Repetition tw twc = permutations'repetition' tw twc
space Unique tw twc = tw `chooseRepetition` twc
