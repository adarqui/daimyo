module Daimyo.Proof.Basic (
    proof'even,
    proof'isEven,
    proof'odd,
    proof'isOdd,
    proof'multiply'odd'odd,
    proof'multiply'odd'odd',
    proof'multiply'odd'odd'2,
    ks_proof'1,
    ks_proof'2,
    ks_proof'3
) where

import Daimyo.Proof.KnowShow

{-
Definition. An integer a is an even integer provided that there exists an
integer n such that a = 2n. An integer a is an odd integer provided there
exists an integer n such that a = 2n + 1.
-}


{-
    for all n, 2n is even
    y = n/2
-}

proof'even n = n/2

proof'isEven n =
    let
        n' = proof'even n
    in
        ceiling n' == floor n'
    
{-
    for all n, 2n+1 is odd
    y = 2n+1
    y/2 = n + 1
    y/2 - 1 = n
-}

proof'odd n = n/2 - 1

proof'isOdd n =
    let
        n' = proof'odd n
    in
        ceiling n' /= floor n'

{-
    Proposition. If x and y are odd integers, then x * y is an odd integer.

    (2m+1)(2m+1) = 4n^2 + 4n + 1 = odd
-}

proof'multiply'odd'odd x y
    | proof'isOdd x && proof'isOdd y = proof'isOdd (x*y)
    | otherwise = error "x & y must both be odd"

proof'multiply'odd'odd' n =
    let
        r = (2*n + 1) * (2*n + 1)
        r' = ((4*(n^2)) + (2*n) + (2*n) + 1)
        r'' = ((4*(n^2)) + (4*n) + 1)
    in
        r''

{-
    Proposition. If x and y are odd integers, then x * y is an odd integer.

    odd = 2m + 1

    (2m + 1) * (2n + 1)
    = 4mn + 2m + 2n + 1
    = 2*(2mn + m + n) + 1
-}

proof'multiply'odd'odd'2 m n
    | proof'isOdd m && proof'isOdd n =
        let
            r = (2*m + 1)*(2*n + 1)
            r' = (4*m*n + 2*m + 2*n + 1)
            r'' = 2*(2*m*n + m + n) + 1
            q = 2*m*n + m + n
            r''' = 2*q + 1
        in
            r'''
    | otherwise = error "x & y must both be odd"

{-
    Proposition: If x is an odd integer and y is an even integer, then x * y is an even integer.

    (2n+1) * (2n) = 4n^2 + 2n

    (2m+1) * (2n)
    = 4mn + 2n =
    = 2(mn + n) =
-}

{-
    Proposition: If x is an odd integer and y is an even integer, then x + y is an odd integer.

    (2n+1) + (2n) = 4n + 1

    (2m+1) + (2n)
    = 2(m + n) + 1
-}

{-
    Proposition: If x and y are even integers, then x * y is an even integer

    2n * 2n = 4n^2

    2m * 2n
    = 2(mn)
-}

{-
    Proposition: If x and y are even integers, then x + y is an even integer

    2n + 2n = 4n

    2m + 2n
    = 2(m + n)
-}

{-
    Proposition: If x is an integer, then x + (-x) is 0

    x + (-x)
    = x - x
    = 0
-}

{-
    Proposition: If x is an integer, then x + (1/x) is 1

    x + (1/x)
    = (x/1) + (1/x)
    = x/x
    = 1
-}


{-
    Know Show table for a proof of:

    Proposition. If x and y are odd integers, then x * y is an odd integer.
-}

ks_proof'1 =
    printit $
    knowShow
        "if x and y are odd integers, then x * y is an odd integer"
        [
            ("x and y are odd integers.", "Hypothesis"),
            ("There exists integers m and n such that x = 2m + 1 and y = 2n + 1", "Definition of an odd integer"),
            ("xy = (2m + 1)(2n + 1)", "Substitution"),
            ("xy = 4mn + 2m + 2n + 1", "Algebra"),
            ("xy = 2(2mn + m + n) + 1", "Algebra"),
            ("(2mn + m + n) is an integer.", "Closure properties of the integers")
        ]
        [
            ("There exists an integer q such that xy = 2q + 1", "Use q = (2mn + m + n)"),
            ("x * y is an odd integer.", "Definition of an odd integer")
        ]

{-
    Know Show table for a proof of:

    Proposition: If x is an even integer and y is an even integer, then x + y is an even integer
-}

ks_proof'2 =
    printit $
    knowShow
        "if x and y are even integers, then x + y is an even integer"
        [
            ("x and y are even integers", "Hypothesis"),
            ("There exists integers m and n such that x = 2m and y = 2n", "Definition of an even integer"),
            ("x+y = 2m + 2n", "Substitution"),
            ("x+y = 2(m + n)", "Algebra, Factorization"),
            ("m+n is an integer", "Closure properties of integers")
        ]
        [
            ("There exists an integer q such that x+y = 2q", "Use q = m + n"),
            ("x + y is an even integer", "Definition of an even integer")
        ]

{-
    Know Show table for a proof of:

    Proposition: If x is an even integer and y is an odd integer, then x + y is an odd integer
-}

ks_proof'3 =
    printit $
    knowShow
        "If x is an even integer and y is an odd integer, then x + y is an odd integer"
        [
            ("x is an even integer, y is an odd integer", "Hypothesis"),
            ("There exists integers m and n such that x = 2m, y = 2n+1", "Definition of even and odd integers"),
            ("x+y = 2m + 2n + 1", "Substitution"),
            ("x+y = 2(m+n) + 1", "Algebra, Factorization"),
            ("m+n is an integer", "Closure property of integers")
        ]
        [
            ("There exists an integer q such that x+y = 2q + 1", "Use q = m + n"),
            ("x + y is an odd integer", "Definition of an odd integer")
        ]
