module Daimyo.Lib.Wave.Calculus.Exercises (
    lim,
    lim'naive,
    lim'map
) where

import Data.List

lim f = lim' f 0 0

lim' f v 10000000 = error "lim max limit"
lim' f v i =
    let r = f i in
    if (r == v)
        then (r, i) 
        else lim' f r (i+1)

lim'naive f =
    foldl' (\acc i -> f i) 0 [1..10000000]

lim'map f =
    map f [1..10000000]

t_lim_ex1 =
    lim (\x -> (x**2 + 1) / (3*(x**3) - (4*x) + 5))

t_lim_ex1' =
    lim (\x -> (x**2) / (3*(x**3)))

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
*Daimyo.Lib.Wave.Calculus.Exercises> 
*Daimyo.Lib.Wave.Calculus.Exercises> t_lim_ex2
(0.3333333333418079,229006.0)
*Daimyo.Lib.Wave.Calculus.Exercises> t_lim_ex2'
(0.3333333333333333,2.0)

Leading Terms Rule efficiency gains.. !
-}
