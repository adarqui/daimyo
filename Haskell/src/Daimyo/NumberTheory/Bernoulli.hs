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

import Daimyo.Algebra.Divisibility
import Daimyo.NumberTheory.Prime
import Daimyo.NumberTheory.Factorial
import Daimyo.NumberTheory.Constants.E

bernoullis = [ bernoulli x | x <- [1..] ]

bernoulli x = bm x


{-
    implement this: http://wstein.org/projects/168/kevin_mcgown/bernproj.pdf
-}

k :: Double -> Double
k m = (2*fac m)/((2*pi)**m)


d :: Double -> Double
d m =
    let
        divs = divisors 50
        divsp1 = map succ divs
        p = filter isPrime divsp1
    in
        fromIntegral (product p) :: Double


n :: Double -> Double -> Double -> Double
n m' k' d' =
    let
        v = ceiling $ (k'*d')**(1/(m'-1))
    in
        fromIntegral v :: Double


z :: Double -> Double -> Double
z m n' =
    let
        p = takeWhile (<=n') $ map (\p' -> fromIntegral p' :: Double) primes
        v = map (\p' -> (1 - p'**(-m))**(-1)) p
    in
        product v


a :: Double -> Double -> Double -> Double -> Double
a m d' k' z' = ((-1)**(m/2+1))*(fromIntegral (ceiling(d'*k'*z')) :: Double)

      
bm 0 = 1
bm 1 = -(1/2)
bm m =
    if (m >= 3 && odd (floor m))
        then 0
        else fst $ bm' m
        

bm' m =
    let
        k' = k m
        d' = d m
        n' = n m k' d'
        z' = z m n'
        a' = a m k' d' z'
    in
        (a' / d',(k',d',n',z',a'))


t_b50'a = 495057205241079648212477525 / 66
t_b50'b = bm 50
