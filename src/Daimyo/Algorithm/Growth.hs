module Daimyo.Algorithm.Growth (
  Growth (..),
  gaps,
  gapsN,
  gapsNSquared,
  gapsLog,
  gapsLogLog,
  gapsFac,
  gapsFib,
  gaps2Expn,
  gapsSin,
  gapsCos,
  gapsTan,
  gapsPrimes,
  growthN,
  growthNSquared,
  growthLog,
  growthLogLog,
  growthFac,
  growth2Expn,
  growthFib,
  growthSin,
  growthCos,
  growthTan,
  growthPrimes,
  growthFunctions,
  testGrowthFunctionResults
) where

import           Daimyo.NumberTheory.Factorial
import           Daimyo.NumberTheory.Fibonacci
import           Daimyo.NumberTheory.Prime

-- | Growth
--
data Growth = Growth {
    _name     :: String,
    _growthFn :: [Double] -> [Double]
}

-- | gaps
--
-- >>> gaps [1,2,3,4] :: [Int]
-- [1,1,1]
--
-- >>> gaps [3,6,12,24] :: [Int]
-- [3,6,12]
--
gaps :: Num t => [t] -> [t]
gaps []         = []
gaps (_:[])     = []
gaps (ij:ik:is) = (ik - ij) : gaps (ik : is)

-- | gapsN
--
gapsN :: Num t => [t] -> [t]
gapsN interval = gaps [ n | n <- interval ]

-- | gapsNSquared
--
gapsNSquared :: Num t => [t] -> [t]
gapsNSquared interval = gaps [ n*n | n <- interval ]

-- | gapsLog
--
gapsLog :: Floating t => [t] -> [t]
gapsLog interval = gaps [ log n | n <- interval ]

-- | gapsLogLog
--
gapsLogLog :: Floating t => [t] -> [t]
gapsLogLog interval = gaps [ log (log n) | n <- interval ]

-- | gapsFac
--
gapsFac :: (Num t, Eq t) => [t] -> [t]
gapsFac interval = gaps [ factorial n | n <- interval ]

-- | gapsFib
--
-- using this method because fibonacci is so slow
--
gapsFib :: RealFrac a => [a] -> [Double]
gapsFib interval = gaps fibs'
  where
    fibs' = map (\n -> fromIntegral (fibonacciNumbers !! (truncate n)) :: Double) interval

-- | gaps2Expn
--
gaps2Expn :: Floating t => [t] -> [t]
gaps2Expn interval = gaps [ 2**n | n <- interval ]

-- | gapsSin
--
gapsSin :: Floating t => [t] -> [t]
gapsSin interval = gaps [ sin n | n <- interval ]

-- | gapsCos
--
gapsCos :: Floating t => [t] -> [t]
gapsCos interval = gaps [ cos n | n <- interval ]

-- | gapsTan
--
gapsTan :: Floating t => [t] -> [t]
gapsTan interval = gaps [ tan n | n <- interval ]

-- | gapsPrimes
--
gapsPrimes :: RealFrac a => [a] -> [Double]
gapsPrimes interval = gaps indexedPrimes
  where
    indexedPrimes = map (\n -> fromIntegral (primes !! (truncate n)) :: Double) interval

growthN        = Growth "n" gapsN
growthNSquared = Growth "n^2" gapsNSquared
growthLog      = Growth "logn" gapsLog
growthLogLog   = Growth "log(logn)" gapsLogLog
growthFac      = Growth "n!" gapsFac
growthFib      = Growth "fib" gapsFib
growth2Expn    = Growth "2^n" gaps2Expn
growthSin      = Growth "sin" gapsSin
growthCos      = Growth "cos" gapsCos
growthTan      = Growth "tan" gapsTan
growthPrimes   = Growth "primes" gapsPrimes

growthFunctions :: [Growth]
growthFunctions =
  [
    growthN,
    growthNSquared,
    growthLog,
    growthLogLog,
    growthFac,
    growthFib,
    growth2Expn,
    growthSin,
    growthCos,
    growthTan,
    growthPrimes
  ]

testGrowthFunctionResults :: Num b => [b] -> [b] -> [b]
testGrowthFunctionResults ns gs = map (\(n,g) -> abs (n - g)) nsgs
  where
    nsgs = zip ns gs
