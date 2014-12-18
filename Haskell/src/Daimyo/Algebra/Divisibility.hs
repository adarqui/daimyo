module Daimyo.Algebra.Divisibility (
    n2de,
    n2de'k,
    de2n,
    number'to'decimal'expansion,
    decimal'expansion'to'number,
    even',
    odd',
    is'even,
    divisible'by'2,
    divisible'by'5,
    divisible'by'3,
    divisible'by'9,
    fraction'to'de
) where


import Daimyo.String
import Daimyo.Algebra.Fraction


number'to'decimal'expansion :: (Show a, Num a) => a -> [Int]
number'to'decimal'expansion n = map (\n' -> read (n':[]) :: Int) $ show n


decimal'expansion'to'number :: [Int] -> Int
decimal'expansion'to'number de =
    let
        reversed = reverse de
    in
        sum $ map (\(n,k) -> n*(10^k)) $ zip reversed [0..]


n2de = number'to'decimal'expansion
de2n = decimal'expansion'to'number
n2de'k n k = take k $ n2de n


fraction'to'de num den =
    let
        (r, d) = split '.' (show (num/den))
        map' xs = map (\n' -> read (n':[]) :: Int) xs
    in
        (map' r, map' d)


-- what if this was the only way to test if numbers were odd or even?
even' 0 = True
even' 2 = True
even' 4 = True
even' 6 = True
even' 8 = True
even' _ = False

odd' = not . even'
        

{-
    So a is even if and only if its last digit a0 is even.
-}

is'even n =
    let
        a0 = last $ n2de n
    in
        even a0


divisible'by'2 = is'even



{-
    Thus a is divisible by 5 if and only if a0 is, which is the case precisely when a0 is 0 or 5. Next let's look at divisibility by 3 and 9.
-}

divisible'by'5 n =
    let
        a0 = last $ n2de n
    in
        if (a0 == 0 || a0 == 5)
            then True
            else False
{-
    A natural number a is divisible by 3 if and only if the sum of its digits is divisible by 3.
    A natural number a is divisible by 9 if and only if the sum of its digits is divisible by 9.
-}

divisible'by'3 = divisible'by'3or9 3
divisible'by'9 = divisible'by'3or9 9

divisible'by'3or9 divisor n =
    let
        de = n2de n
    in
        ((sum de) `mod` divisor) == 0


{-
    Let a be a natural number. Write a = 10b + a0, where 0 ≤ a0 < 10. Then a is divisible by 7 if and only if b − 2a0 is divisible by 7.
-}

divisible'by'7 n =
    let
        a0x21 = 21 * (last $ n2de n)
    in
        a0x21 `mod` 7 == 0


t_div'1 n =
    let
        de = n2de n
        a0 = last de
    in
        n == a0 `mod` 2

t_div'7 = divisible'by'7 98

t_fraction = fraction'to'de 1 7
