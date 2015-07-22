module Daimyo.Algebra.RepeatedDecimal (
  dec2s
) where

import Daimyo.Number

--  unfinished.. floating point issues have me stumped atm.
-- 0.544444...
-- n = 0.54....
-- 10n = 5.44444...
-- 100n = 54.4444....
-- 100n = 54.444...
-- 10n =  5.444...
-- 90n = 49
-- n = 49/90

dec2s n = show n

t_rep'1 = 4/11
t_rep'2 = 7/9
t_rep'3 = 1/9
