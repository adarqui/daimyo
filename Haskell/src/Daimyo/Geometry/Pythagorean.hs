module Daimyo.Geometry.Pythagorean (
    module Daimyo.Geometry.Triangle,
    pythagorean
) where

import Daimyo.Geometry.Triangle

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
