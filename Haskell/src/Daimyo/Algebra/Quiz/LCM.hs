{-# LANGUAGE RecordWildCards #-}

-- lots of redundant code

module Daimyo.Algebra.Quiz.LCM (
    LCM (..),
    ask,
    new
) where

import System.Random
import Text.Printf

data LCM a = LCM {
    min :: a,
    max :: a
} deriving (Show)

new min' max' = LCM min' max'

ask v = do
    g <- getStdGen
    ask' g v

ask' g v@LCM{..} = do
    let (x:y:[]) = take 2 $ randomRs (min, max) g
    putStrLn $ printf "lcm (%d, %d)" x y
    recv $ lcm x y
    g <- newStdGen
    ask' g v

recv ans = do
    v <- readLn :: IO Int
    if (v /= ans)
        then do
            putStrLn $ printf "Wrong! (%d)" ans
        else do
            putStrLn "Correct!"
