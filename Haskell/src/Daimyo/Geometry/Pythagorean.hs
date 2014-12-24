module Daimyo.Geometry.Pythagorean (
    module Daimyo.Geometry.Triangle,
    pythagorean,
    triples'even,
    triples'odd,
    triples
) where

import Daimyo.Geometry.Triangle
import Daimyo.NumberTheory.Factor
import Daimyo.List.Misc

pythagorean :: Maybe Side -> Maybe Side -> Maybe Side -> Triangle
pythagorean (Just (A a)) (Just (B b)) Nothing = Triangle a b (solve'c a b)
pythagorean Nothing (Just (B b)) (Just (C c)) = Triangle (solve'a c b) b c
pythagorean (Just (A a)) Nothing (Just (C c)) = Triangle a (solve'b c a) c
pythagorean _ _ _ = error "Invalid"

-- c^2 = a^2 + b^2
solve'c a b = sqrt ((a**2) + (b**2))
-- a^2 = c^2 - b^2
solve'a c b = sqrt ((c**2) - (b**2))
-- b^2 = c^2 - a^2
solve'b c a = sqrt ((c**2) - (a**2))

t_pythagorean'25 = pythagorean (Just (A 3)) (Just (B 4)) Nothing
t_pythagorean'3 = pythagorean Nothing (Just (B 4)) (Just (C 5))
t_pythagorean'4 = pythagorean (Just (A 3)) Nothing (Just (C 5))
t_pythagorean'irrat = pythagorean (Just (A 1)) (Just (B 1)) Nothing

{-
    pythagorean triple generator

    (2uv, u^2-v^2, u^2+v^2)

    eek
-}

triples'even = 
    let
        evens = [ 2*n | n <- [1..] ]
    in
    filter (\(x,_,_) -> x /= 0) $ concatMap (\n ->
        let
            b = n
            facs = factor'pairs' (b `div` 2)
        in
            map (\(m,n) -> (m^2-n^2, b, m^2+n^2)) facs
        ) evens

triples'odd =
    let
        odds = [ 2*n-1 | n <- [0..] ]
    in
    filter (\(x,_,_) -> x /= 0) $ concatMap (\n ->
        let
            b = n
            facs = factor'pairs' b
        in
            map (\(m,n) -> ((m^2-n^2)`div`2, b, (m^2+n^2)`div`2)) facs
        ) odds

triples = interleave triples'even triples'odd
