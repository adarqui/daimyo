module Math (
 fib,
 fact
) where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
