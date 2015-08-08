{-# LANGUAGE RecordWildCards #-}

-- lots of redundant code

module Daimyo.Algebra.Quiz.ASMD (
    ASMD (..),
    Op (..),
--    ask,
    new
) where

import System.Random
import Text.Printf

data Op = Add | Sub | Mul | Div deriving (Show, Read, Eq, Enum, Bounded)

data ASMD a = ASMD {
    min :: a,
    max :: a,
    terms :: Int,
    ops :: [Op]
} deriving (Show)

new min' max' terms' ops' = ASMD min' max' terms' ops'

{-
ask v = do
    g <- getStdGen
    ask' g v

ask' g v@GCD{..} = do
    let (x:y:[]) = take 2 $ randomRs (min, max) g
    putStrLn $ printf "gcd (%d, %d)" x y
    recv $ gcd x y
    g <- newStdGen
    ask' g v

recv ans = do
    v <- readLn :: IO Int
    if (v /= ans)
        then do
            putStrLn $ printf "Wrong! (%d)" ans
        else do
            putStrLn "Correct!"
-}
