module Daimyo.NumberTheory.Bernoulli (
  bernoullis,
  bernoulli,
  k,
  d,
  n,
  z,
  a,
  bm,
  bm'
) where

import           Daimyo.Algebra.Divisibility
import           Daimyo.NumberTheory.Factorial
import           Daimyo.NumberTheory.Prime

-- | bernoullis
--
-- >>> take 5 bernoullis
-- [-0.5,0.16666666666666666,0.0,-4.5454545454545456e-2,0.0]
--
bernoullis :: [Double]
bernoullis = [ bernoulli x | x <- [1..] ]

-- | bernoulli
--
-- >>> bernoulli 10
-- 7.575757575757576e-2
--
bernoulli :: Double -> Double
bernoulli x = bm x


--
-- below implements this: http://wstein.org/projects/168/kevin_mcgown/bernproj.pdf
--

-- | k
--
k :: Double -> Double
k m = (2*factorial m)/((2*pi)**m)

-- | d
--
d :: Double -> Double
d _ = fromIntegral $ product p
  where
    divs   = divisors 50
    divsp1 = map succ divs
    p      = filter isPrime divsp1

-- | n
--
n :: Double -> Double -> Double -> Double
n m' k' d' = fromIntegral v
  where
    v = ceiling ((k'*d')**(1/(m'-1))) :: Integer

-- | z
--
z :: Double -> Double -> Double
z m n' = product v
  where
    p = takeWhile (<=n') $ map fromIntegral primes
    v = map (\p' -> (1 - p'**(-m))**(-1)) p

-- | a
--
a :: Double -> Double -> Double -> Double -> Double
a m d' k' z' = ((-1)**(m/2+1))*(fromIntegral (ceiling (d'*k'*z') :: Integer))

-- | bm
--
bm :: Double -> Double
bm 0 = 1
bm 1 = -(1/2)
bm m
  | m >= 3 && odd (floor m :: Integer) = 0
  | otherwise               = fst $ bm' m

-- | bm'
--
bm' :: Double -> (Double, (Double, Double, Double, Double, Double))
bm' m = (a' / d',(k',d',n',z',a'))
  where
  k' = k m
  d' = d m
  n' = n m k' d'
  z' = z m n'
  a' = a m k' d' z'
