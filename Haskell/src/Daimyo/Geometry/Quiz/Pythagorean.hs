{-# LANGUAGE RecordWildCards #-}

-- lots of redundant code

module Daimyo.Geometry.Quiz.Pythagorean (
    Pythagorean (..),
    Side (..),
    ask,
    new,
    solveFor
) where

import System.Random
import Text.Printf

data Side = A | B | C deriving (Show, Eq, Enum, Read)

data Pythagorean a = Pythagorean {
    min :: a,
    max :: a
} deriving (Show)

new min' max' = Pythagorean min' max'

ask v = do
    g <- getStdGen
    ask' g v

ask' g v@Pythagorean{..} = do
    let (a:b:[]) = take 2 $ randomRs (min, max) g
    let (c, _) = randomR ('A', 'C') g
    let (s, r) = solveFor (read (c:[]) :: Side) (a^2) (b^2) (a^2 + b^2)
    putStrLn $ printf s
    recv r
    g <- newStdGen
    ask' g v

recv ans = do
    v <- readLn :: IO Int
    if (v /= ans)
        then do
            putStrLn $ printf "Wrong! (%d)" ans
        else do
            putStrLn "Correct!"

solveFor :: Side -> Int -> Int -> Int -> (String, Int)
solveFor A a b c = (printf "c^2=%d, b^2=%d, a^2=?" c b, a)
solveFor B a b c = (printf "c^2=%d, a^2=%d, b^2=?" c a, b)
solveFor C a b c = (printf "c^2=? a^2=%d, b^2=%d" a b, c)
