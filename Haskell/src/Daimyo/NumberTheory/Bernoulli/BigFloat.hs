module Daimyo.NumberTheory.Bernoulli.BigFloat (
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
import           Daimyo.Algebra.Exponent
import           Daimyo.NumberTheory.Factorial
import           Daimyo.NumberTheory.Prime
import           Data.Number.BigFloat

-- | bernoullis
--
-- bernoulli numbers?
--
-- >>> take 3 bernoullis
-- [-5.00000000000000000000000000000000000000000000000000e-1,1.66666666666666666666666666666666666666666666666667e-1,0.00000000000000000000000000000000000000000000000000e0]
--
bernoullis :: [BigFloat Prec50]
bernoullis = [ bernoulli (fromIntegral x) | x <- [1..] :: [Int] ]

-- | bernoulli
--
-- the nth bernoulli number?
--
-- >>> bernoulli 500
-- -1.98382953521271137829146935666600136587268260390429e735
--
bernoulli :: BigFloat Prec50 -> BigFloat Prec50
bernoulli x = bm x

--
-- HOLY SHIT!
--

--
-- below, this is implemented: http://wstein.org/projects/168/kevin_mcgown/bernproj.pdf
--

-- | k
--
k :: BigFloat Prec50 -> BigFloat Prec50
k m = (2*fac m)/((2*pi)**m)

-- | d
--
d :: BigFloat Prec50 -> BigFloat Prec50
d _ = fromIntegral (product p) :: BigFloat Prec50
  where
    divs   = divisors 50
    divsp1 = map succ divs
    p      = filter isPrime divsp1

-- | n
--
n :: BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50
n m' k' d' = fromIntegral v :: BigFloat Prec50
  where
    v = (ceiling $ (k'*d')**(1/(m'-1)) :: Integer)

-- | z
--
z :: BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50
z m n' = product v
  where
    p = takeWhile (<=n') $ map (\p' -> fromIntegral p') primes
    v = map (\p' -> (1 - p'**(-m))**(-1)) p

-- | a
--
a :: BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50
a m d' k' z' = (negPow (-1) (m/2+1))*(fromIntegral (ceiling(d'*k'*z') :: Integer))

-- | bm
--
-- >>> bm 50
-- 7.50086674607696436685572007575757575757575757575758e24
--
bm :: BigFloat Prec50 -> BigFloat Prec50
bm 0 = 1
bm 1 = -(1/2)
bm m
  | m >= 3 && odd (floor m :: Integer) = 0
  | otherwise               = fst $ bm' m

-- | bm'
--
-- whoa.
--
bm' :: BigFloat Prec50 -> (BigFloat Prec50, (BigFloat Prec50, BigFloat Prec50, BigFloat Prec50, BigFloat Prec50, BigFloat Prec50))
bm' m = (a' / d',(k',d',n',z',a'))
  where
    k' = k m
    d' = d m
    n' = n m k' d'
    z' = z m n'
    a' = a m k' d' z'
