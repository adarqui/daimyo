module Daimyo.Calculus.Limit (
  limit,
  limitNaive,
  limitMap
) where

import Data.List

limit_max = 100000000000000000
limit_step = 1000
limit_range = [0,limit_step..limit_max]

limit f = limit' f 0 0

limit' f v 100000000000000000 = (v, limit_max)
limit' f v i =
    let r = f i in
    if (r == v)
        then (r, i)
        else limit' f r (i+limit_step)

limitNaive f =
    foldl' (\acc i -> f i) 0 limit_range

limitMap f =
    map f limit_range

limitDist :: (Double -> Double) -> [Double]
limitDist f =
    dist $ limitMap f

dist :: [Double] -> [Double]
dist [] = [0.0]
dist (x:y:rest) = abs (x-y) : dist rest

t_limit_ex1 =
    limit (\x -> (x**2 + 1) / (3*(x**3) - (4*x) + 5))

t_limit_ex1' =
    limit (\x -> (x**2) / (3*(x**3)))

t_limit_dist_ex1 = take 200 $ limitDist (\x -> (x**2 + 1) / (3*(x**3) - (4*x) + 5))

t_limit_ex2 =
    limit (\x -> (x**3 + 1) / (3*(x**3) - (4*x) + 5))

t_limit_ex2' =
    limit (\x -> (x**3) / (3*(x**3)))

t_limit_ex3 =
    limit (\x -> (2**(-x) + 3))

t_limit_ex4 =
    limit (\x -> (x+1) / (2*x + 1))

t_limit_ex5 =
    limit (\x -> (4 + (1/(2**x))))

{-
*Daimyo.Calculus.Exercises>
*Daimyo.Calculus.Exercises> t_limit_ex2
(0.3333333333418079,229006.0)
*Daimyo.Calculus.Exercises> t_limit_ex2'
(0.3333333333333333,2.0)

Leading Terms Rule efficiency gains.. !
-}
