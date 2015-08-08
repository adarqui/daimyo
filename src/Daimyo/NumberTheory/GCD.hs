{-
    sources:
        GCD
            http://en.wikipedia.org/wiki/Greatest_common_divisor
-}

module Daimyo.NumberTheory.GCD (
    gcd,
    gcd',
    gcd'',
    gcds,
    gcd'Explain,
    t_gcd_explain,
    gcd'Mod'Explain,
    t_gcd_mod_explain
) where

import Daimyo.Print

import Data.List
import Control.Monad
import Control.Monad.Writer
import Text.Printf

{-
    GCD
-}

gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

gcf = gcd'
hcf = gcd'
gcm = gcd'
hcd = gcd'

gcd'' a b
    | a == b = a
    | a > b = gcd (a-b) b
    | b > a = gcd a (b-a)

{-
    gcd of multiple integers
-}

gcds = foldl' gcd 0

{-
    gcd using subtraction
-}

gcd'Explain :: Int -> Int -> IO (Int, [String])
gcd'Explain a b = do
    (r, w) <- runWriterT (gcd'Explain' a b)
    return (r, w)

gcd'Explain' :: Int -> Int -> WriterT [String] IO Int
gcd'Explain' a b = do
    tell [printf "gcd (a=%d) (b=%d)" a b]
    if (a == b)
        then do
            tell [printf "(a=%d) == (b=%d)" a b]
            return a
        else do
            if (a > b)
                then do
                    tell [printf "(a=%d) > (b=%d)" a b]
                    tell [printf "gcd (a=%d - b=%d) (b=%d)" a b b]
                    gcd'Explain' (a-b) b
                else do
                    tell [printf "(b=%d) > (a=%d)" b a]
                    tell [printf "gcd (a=%d) (a=%d - b=%d)" a a b]
                    gcd'Explain' a (b-a)

t_gcd_explain a b = do
    (r, w) <- gcd'Explain a b
    mapM_ putStrLn w
    putStrLn $ show r

{-
    gcd using modulus
-}

gcd'Mod'Explain :: Int -> Int -> IO (Int, [String])
gcd'Mod'Explain a b = do
    (r, w) <- runWriterT (gcd'Mod'Explain' a b)
    return (r, w)

gcd'Mod'Explain' :: Int -> Int -> WriterT [String] IO Int
gcd'Mod'Explain' a b = do
    tell [printf "gcd (a=%d) (b=%d)" a b]
    if (a == b || b == 0)
        then do
            tell [printf "match: (a=%d == b=%d) || (b=%d == 0)" a b b]
            tell [printf "= %d" a]
            return a
        else do
            tell [printf "gcd (b=%d) (a=%d mod b=%d)" b a b]
            gcd'Mod'Explain' b (a `mod` b)

t_gcd_mod_explain a b = do
    (r, w) <- gcd'Mod'Explain a b
    mapM_ putStrLn w
    putStrLn $ show r
