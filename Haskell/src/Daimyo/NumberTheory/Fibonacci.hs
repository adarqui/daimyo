module Daimyo.NumberTheory.Fibonacci (
    fibonacci,
    fibonacciNumbers,
    fibonacciGCD
) where

fibonacciNumbers = 0 : 1 : 1 : fibonacciNumbers' 1 1
fibonacciNumbers' p c = (p + c) : fibonacciNumbers' c (p+c)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibonacciGCD m n = fibonacci (gcd m n )
