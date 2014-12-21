module Daimyo.ConcreteMathematics.Recurrences (
) where

import Daimyo.ConcreteMathematics.Sums

sum'example n = sigma 0 n (\ak -> ak)

recurrence'example 0 = 0
recurrence'example n = n + recurrence'example (n-1)
