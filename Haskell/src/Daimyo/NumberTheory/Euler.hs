module Daimyo.NumberTheory.Euler (
    phi,
    euler,
    znz
) where

{-
    phi (euler's function)

    For n in N, let phi (n) = #{a in N : a <= n and gcd (a, n) = 1 }
-}

phi n = length $ [ a | a <- [1..n], a <= n && gcd a n == 1 ]

euler x n
    | gcd x n == 1 = x^(phi n) `mod` n
    | otherwise = error "gcd x n /= 1"

{-
    Z/nZ
-}
znz x n = x `mod` n

t_phi = [phi 1, phi 2, phi 5, phi 12, phi 2007]
t_euler =
    let
        n = 20
        k = phi n
    in
        [ (znz x n)^k | x <- [1..n], gcd x n == 1 ]
    -- makes no sense how it would be: [1, 1, 1, 1, 1, 1, 1, 1] .. ??? map (\i -> i^(phi k)) [1,3,7,9,11,13,17,19]
