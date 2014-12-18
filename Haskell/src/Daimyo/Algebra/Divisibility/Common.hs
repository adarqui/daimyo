module Daimyo.Algebra.Divisibility.Common (
    common'divisor
) where

common'divisor d a b =
    (a `mod` d == 0 && b `mod` d == 0)

t_common'divisor = common'divisor 2 10 12
