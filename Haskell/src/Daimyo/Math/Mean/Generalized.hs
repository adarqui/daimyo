module Daimyo.Math.Mean.Generalized (
    generalized'mean,
    generalized,
    f'mean
) where

import Data.List

import Daimyo.Math.Mean.Arithmetic
import Daimyo.Math.Mean.Geometric
import Daimyo.Math.Mean.Harmonic

generalized'mean m l =
    let
        n = length l
        sum' = foldl' (\acc y -> acc + (y**m)) 0 l
    in
        (sum' / fromIntegral n)**(1/m)

generalized = generalized'mean

f'mean f l =
    let
        n = length l
        sum' = foldl' (\acc y -> acc + f y) 0 l
    in
        f $ sum' / fromIntegral n

t_nums = [4,36,45,50,75]

t_generalized'1 =
    (
        [generalized 2 t_nums, generalized 1 t_nums, generalized 0 t_nums, generalized (-1) t_nums],
        [0, arithmetic t_nums, geometric t_nums, harmonic t_nums]
    )

t_f'mean'1 =
    (
        [f'mean (\x -> x) t_nums, f'mean (\x -> log x) t_nums, f'mean (\x -> 1/x) t_nums],
        [arithmetic t_nums, geometric t_nums, harmonic t_nums]
    )
