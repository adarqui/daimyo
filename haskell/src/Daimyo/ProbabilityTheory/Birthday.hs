module Daimyo.ProbabilityTheory.Birthday (
    birthday'approx
) where

import Daimyo.NumberTheory.Constants.E

birthday'approx n k = 1 - e**(-((n**2)/(2*k)))

t_birthday'approx = [birthday'approx (2**36) (2**122), birthday'approx (2**41) (2**122), birthday'approx (2**46) (2**122)]
