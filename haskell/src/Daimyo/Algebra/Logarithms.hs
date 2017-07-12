--  sources:
--  - http://en.wikipedia.org/wiki/Logarithm

module Daimyo.Algebra.Logarithms (
) where

data Log b x
  = Log b x
  deriving (Show)

-- logb(y) = x <-> b^x=y
-- log10(100) = 2 <-> 10^2=100
-- log2(16) = 4 <-> 2^4=16
-- log2(1/2) = -1 <-> 2^(-1)=1/2
-- logb(b) = 1 <-> b^1=b
-- logb(1) = 0 <-> b^0=1
