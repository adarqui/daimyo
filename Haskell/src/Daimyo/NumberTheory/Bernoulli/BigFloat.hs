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

import Daimyo.Algebra.Exponent
import Daimyo.Algebra.Divisibility
import Daimyo.NumberTheory.Prime
import Daimyo.NumberTheory.Factorial
import Daimyo.NumberTheory.Constants.E

import Data.Number.Fixed
import Data.Number.BigFloat

bernoullis = [ bernoulli (fromIntegral x :: BigFloat Prec50) | x <- [1..] ]

bernoulli x = bm x


{-
    implement this: http://wstein.org/projects/168/kevin_mcgown/bernproj.pdf
-}

k :: BigFloat Prec50 -> BigFloat Prec50
k m = (2*fac m)/((2*pi)**m)


d :: BigFloat Prec50 -> BigFloat Prec50
d m =
    let
        divs = divisors 50
        divsp1 = map succ divs
        p = filter isPrime divsp1
    in
        fromIntegral (product p) :: BigFloat Prec50


n :: BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50
n m' k' d' =
    let
        v = ceiling $ (k'*d')**(1/(m'-1))
    in
        fromIntegral v :: BigFloat Prec50


z :: BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50
z m n' =
    let
        p = takeWhile (<=n') $ map (\p' -> fromIntegral p' :: BigFloat Prec50) primes
        v = map (\p' -> (1 - p'**(-m))**(-1)) p
    in
        product v


a :: BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50 -> BigFloat Prec50
a m d' k' z' = (negPow (-1) (m/2+1))*(fromIntegral (ceiling(d'*k'*z')) :: BigFloat Prec50)

      
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
