module Daimyo.Calculus.Exercises (
    lim,
    lim'naive,
    lim'map
) where

import Data.List

lim_max = 100000000000000000
lim_step = 1000
lim_range = [0,lim_step..lim_max]

lim f = lim' f 0 0

lim' f v 100000000000000000 = (v, lim_max)
lim' f v i =
    let r = f i in
    if (r == v)
        then (r, i) 
        else lim' f r (i+lim_step)

lim'naive f =
    foldl' (\acc i -> f i) 0 lim_range

lim'map f =
    map f lim_range

lim'dist :: (Double -> Double) -> [Double]
lim'dist f =
    dist $ lim'map f

dist :: [Double] -> [Double]
dist [] = [0.0]
dist (x:y:rest) = abs (x-y) : dist rest

t_lim_ex1 =
    lim (\x -> (x**2 + 1) / (3*(x**3) - (4*x) + 5))

t_lim_ex1' =
    lim (\x -> (x**2) / (3*(x**3)))

t_lim_dist_ex1 = take 200 $ lim'dist (\x -> (x**2 + 1) / (3*(x**3) - (4*x) + 5))

t_lim_ex2 =
    lim (\x -> (x**3 + 1) / (3*(x**3) - (4*x) + 5))

t_lim_ex2' =
    lim (\x -> (x**3) / (3*(x**3)))

t_lim_ex3 =
    lim (\x -> (2**(-x) + 3))

t_lim_ex4 =
    lim (\x -> (x+1) / (2*x + 1))

t_lim_ex5 =
    lim (\x -> (4 + (1/(2**x))))

{-
*Daimyo.Calculus.Exercises> 
*Daimyo.Calculus.Exercises> t_lim_ex2
(0.3333333333418079,229006.0)
*Daimyo.Calculus.Exercises> t_lim_ex2'
(0.3333333333333333,2.0)

Leading Terms Rule efficiency gains.. !
-}
