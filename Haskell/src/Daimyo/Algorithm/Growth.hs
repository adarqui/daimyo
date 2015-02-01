-- MAJOR TODO FIXME: turn these into BIG NUMS. Double is too small.

module Daimyo.Algorithm.Growth (
    Growth (..),
    gaps,
    gaps'n,
    gaps'n2,
    gaps'logn,
    gaps'log'logn,
    gaps'nfac,
    gaps'2expn,
    gaps'sin,
    gaps'cos,
    growth'n,
    growth'n2,
    growth'logn,
    growth'log'logn,
    growth'nfac,
    growth'2expn,
    growth'sin,
    growth'cos,
    growthFunctions,
    testGrowthFunctionResults
) where

import Daimyo.NumberTheory.Factorial

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
gaps'2expn interval = gaps [ 2**n | n <- interval ]
gaps'sin interval = gaps [ sin n | n <- interval ]
gaps'cos interval = gaps [ cos n | n <- interval ]

growth'n = Growth "n" gaps'n
growth'n2 = Growth "n^2" gaps'n2
growth'logn = Growth "logn" gaps'logn
growth'log'logn = Growth "log(logn)" gaps'log'logn
growth'nfac = Growth "n!" gaps'nfac
growth'2expn = Growth "2^n" gaps'2expn
growth'sin = Growth "sin" gaps'sin
growth'cos = Growth "cos" gaps'cos

growthFunctions =
    [
        growth'n,
        growth'n2,
        growth'logn,
        growth'log'logn,
        growth'nfac,
        growth'2expn,
        growth'sin,
        growth'cos
    ]

testGrowthFunctionResults ns gs = 
    let
        nsgs = zip ns gs
    in
        map (\(n,g) -> abs (abs n - abs g)) nsgs
