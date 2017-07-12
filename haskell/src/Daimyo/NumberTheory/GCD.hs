-- sources:
-- http://en.wikipedia.org/wiki/Greatest_common_divisor

module Daimyo.NumberTheory.GCD (
  gcd,
  gcd',
  gcd'',
  gcf,
  hcf,
  gcm,
  hcd,
  gcds,
  gcdExplain,
  t_gcdExplain,
  gcdModExplain,
  t_gcdModExplain
) where

import Daimyo.Print
import Data.List
import Control.Monad
import Control.Monad.Writer
import Text.Printf

-- | gcd'
--
gcd' :: Integral a => a -> a -> a
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

-- | gcds: gcd of multiple integers
--
gcds :: Integral a => [a] -> a
gcds = foldl' gcd 0

-- | gcdExplain: gcd using subtraction
--
gcdExplain :: Int -> Int -> (Int, [String])
gcdExplain a b = runWriter (go a b)
  where
  go :: Int -> Int -> Writer [String] Int
  go a b = do
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
            go (a-b) b
          else do
            tell [printf "(b=%d) > (a=%d)" b a]
            tell [printf "gcd (a=%d) (a=%d - b=%d)" a a b]
            go a (b-a)

t_gcdExplain :: Int -> Int -> IO ()
t_gcdExplain a b = do
  mapM_ putStrLn w
  putStrLn $ show r
  where
    (r, w) = gcdExplain a b

-- | gcdModExplain: gcd using modulus
--
gcdModExplain :: Int -> Int -> (Int, [String])
gcdModExplain a b = runWriter (go a b)
  where
  go :: Int -> Int -> Writer [String] Int
  go a b = do
    tell [printf "gcd (a=%d) (b=%d)" a b]
    if (a == b || b == 0)
      then do
        tell [printf "match: (a=%d == b=%d) || (b=%d == 0)" a b b]
        tell [printf "= %d" a]
        return a
      else do
        tell [printf "gcd (b=%d) (a=%d mod b=%d)" b a b]
        go b (a `mod` b)

t_gcdModExplain :: Int -> Int -> IO ()
t_gcdModExplain a b = do
  mapM_ putStrLn w
  putStrLn $ show r
  where
    (r, w) = gcdModExplain a b
