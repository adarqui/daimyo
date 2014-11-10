module Misc.Math (
 fib,
 fact,
 times,
 double,
 induction,
 induction_fact,
 induction_sumInt,
 induction_sumSqr
) where


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)


times :: Int -> Int -> Int
times n m = n * m


double :: Int -> Int
double n = times n 2

induction :: (Num a, Ord a) => a -> (a -> a -> a) -> a -> a
induction base comb n
 | n == 0 = base
 | n > 0 = comb n (induction base comb (n - 1))

induction_fact :: (Num a, Ord a) => a -> a
induction_fact n = induction 1 (*) n

induction_sumInt :: (Num a, Ord a) => a -> a
induction_sumInt n = induction 0 (+) n

induction_sumSqr :: (Num a, Ord a) => a -> a
induction_sumSqr n = induction 0 (\x y -> x * x + y) n
