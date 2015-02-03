-- MAJOR TODO FIXME: turn these into BIG NUMS. Double is too small.

module Daimyo.Algorithm.Growth (
    Growth (..),
    gaps,
    gaps'n,
    gaps'n2,
    gaps'logn,
    gaps'log'logn,
    gaps'nfac,
    gaps'fib,
    gaps'2expn,
    gaps'sin,
    gaps'cos,
    gaps'tan,
    gaps'primes,
    growth'n,
    growth'n2,
    growth'logn,
    growth'log'logn,
    growth'nfac,
    growth'2expn,
    growth'fib,
    growth'sin,
    growth'cos,
    growth'tan,
    growth'primes,
    growthFunctions,
    testGrowthFunctionResults
) where

import Daimyo.NumberTheory.Prime
import Daimyo.NumberTheory.Factorial
import Daimyo.NumberTheory.Fibonacci

import Data.List

data Growth = Growth {
    _name :: String,
    _growthFn :: [Double] -> [Double]
}

gaps [] = []
gaps (ij:[]) = []
gaps (ij:ik:is) = (ik - ij) : gaps (ik : is)

gaps'n interval = gaps [ n | n <- interval ]
gaps'n2 interval = gaps [ n*n | n <- interval ]
gaps'logn interval = gaps [ log n | n <- interval ]
gaps'log'logn interval = gaps [ log (log n) | n <- interval ]
gaps'nfac interval = gaps [ fac n | n <- interval ]

-- using this method because fibonacci is so slow
gaps'fib interval =
    let
        fibs' = map (\n -> fromIntegral (fibonacciNumbers !! (truncate n)) :: Double) interval
    in
        gaps fibs'
gaps'2expn interval = gaps [ 2**n | n <- interval ]
gaps'sin interval = gaps [ sin n | n <- interval ]
gaps'cos interval = gaps [ cos n | n <- interval ]
gaps'tan interval = gaps [ tan n | n <- interval ]
gaps'primes interval =
    let
        -- noob
        primes' = map (\n -> fromIntegral (primes !! (truncate n)) :: Double) interval
    in
        gaps primes'

growth'n = Growth "n" gaps'n
growth'n2 = Growth "n^2" gaps'n2
growth'logn = Growth "logn" gaps'logn
growth'log'logn = Growth "log(logn)" gaps'log'logn
growth'nfac = Growth "n!" gaps'nfac
growth'fib = Growth "fib" gaps'fib
growth'2expn = Growth "2^n" gaps'2expn
growth'sin = Growth "sin" gaps'sin
growth'cos = Growth "cos" gaps'cos
growth'tan = Growth "tan" gaps'tan
growth'primes = Growth "primes" gaps'primes

growthFunctions =
    [
        growth'n,
        growth'n2,
        growth'logn,
        growth'log'logn,
        growth'nfac,
        growth'fib,
        growth'2expn,
        growth'sin,
        growth'cos,
        growth'tan,
        growth'primes
    ]

testGrowthFunctionResults ns gs = 
    let
        nsgs = zip ns gs
    in
        map (\(n,g) -> abs (abs n - abs g)) nsgs
