module Daimyo.Proof.Exercises.Misc (
    equality'tests,
    zero'tests,
    parallel'tests,
    t_table'polynomial'1,
    ks_proof'1,
    ks_proof'2,
    ks_proof'3,
    ks_proof'4,
    ks_proof'5,
) where

import Daimyo.Proof.KnowShow
import Daimyo.Geometry.Line
import Data.Matrix

t_table'polynomial'1 =
    let
        v = [-6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6]
    in
        fromLists $ map (\n -> [n, poly n]) v
    where
        poly m = 3*(m^2) + 4*m + 6


ks_proof'1 =
    printit $ knowShow
        "if m is an odd integer, then 3m^2 + 4m + 6 is an odd integer"
        [
            ("m is an odd integer", "Hypothesis"),
            ("odd integer = 2n+1", "Definition of an odd integer"),
            ("3(2n+1)^2 + 4(2n+1) + 6", "Substitution"),
            ("3(2n+1)(2n+1) + 4(n+1) + 6", "Algebra"),
            ("3(4n^2 + 2n + 2n + 1) + 4n + 4 + 6", "Algebra"),
            ("12n^2 + 4n + 1 + 4n + 4 + 6", "Algebra"),
            ("12n^2 + 8n + 10 + 1", "Algebra"),
            ("2(6n^2 + 4n + 5) + 1", "Algebra, Factorization"),
            ("Let q = 6n^2 + 4n + 5", "Closure under multiplication and addition")
        ]
        [
            ("For all q, 2q+1 is an odd integer", "Definition of an odd integer"),
            ("if m is an odd integer, then 3m^2 + 4m + 6 is an odd integer", "Definition of an odd integer")
        ]

{-
    Proposition. If m is a real number and m, m + 1, and m + 2 are the lengths of the three sides of a right triangle, then m = 3.
-}

ks_proof'2 = 
    printit $ knowShow
        "If m is a real number and m, m + 1, and m + 2 are the lengths of the three sides of a right triangle, then m = 3."
        [
            ("If m is a real number and m, m+1, and m+2 are the lengths of the three sides of a triangle","Hypothesis"),
            ("c^2 = a^2 + b^2", "Definition: pythagorean theorem"),
            ("(m+2)^2 = (m+1)^2 + m^2", ".."),
            ("(m+2)(m+2) = (m+1)(m+1) + m^2", ".."),
            ("m^2 + 4m + 4 = m^2 + 2m + 1 + m^2", ".."),
            ("m^2 + 4m + 4 = 2m^2 + 2m+ 1", ".."),
            ("m^2 - 2m - 3", ".."),
            ("m^2 - 2m - 3 = 0", ".."),
            ("(m-3)(m+1) = 0", ".."),
            ("m = 3, m = -1","..")
        ]
        [
            ("m = 3", "...")
        ]

ks_proof'3 =
    printit $ knowShow
        "If m is an even integer, then m + 1 is an odd integer"
        [
            ("m is an even integer", "Hypothesis"),
            ("2n+1 is an odd integer", "Definition: odd integer"),
            ("2q is an even integer", "Definition: even integer"),
            ("m = 2q", ".."),
            ("2q + 1", "..")
        ]
        [
            ("for some q, m = 2q", ".."),
            ("m+1 is equivalent to 2q+1", "Definition of an odd integer"),
            ("m + 1 is an even integer", "QED")
        ]

ks_proof'4 =
    printit $ knowShow
        "If m is an even integer and n is an integer, then m * n is an even integer."
        [
            ("m is an even integer and n is an integer", ".."),
            ("x is an even integer such that x = 2m", ".."),
            ("y is an integer such that y = 2n or y = 2n+1", ".."),
            ("2(2n) .. Let q = 2n. 2q = even", ".."),
            ("2(2n+1) .. Let q = 2n+1. 2q = even ", "..")
        ]
        [
            ("m * n is an even integer", "QED")
        ]

ks_proof'5 =
    printit $ knowShow
        "if m is an even integer, then 5m + 7 is an odd integer"
        [
            ("m is an even integer", "Hypothesis"),
            ("x = 2m", ".."),
            ("5 (2q) + 7", ".."),
            ("10q + 7", ".."),
            ("2(5q) + 7", "..")
        ]
        [
            ("even + odd = odd", ".."),
            ("5m + 7 is an odd integer", "QED")
        ]

{-
    How can we prove that two numbers are equal?
-}

equality'tests m n =
    (m/n == 1) && (m - n == 0) && (m + n == (m*2)) && (m - n == n - m)

{-
    How can we prove that a real number is equal to zero?
-}

zero'tests n =
    (n + n == n) && (n - n == 0) && (n * 1 == 0)


{-
    How can we prove that two lines are parallel
-}

parallel'tests l1 l2 =
    (m l1 == m l2)


{-
    How can we prove that a triangle is isosceles
-}
