module Daimyo.Delrik.Deutsch (
  v2c,
  m2c,
  qbtZero,
  qbtOne
) where

import Data.List
import Data.Complex

(<*>) :: Num a => a -> Matrix a -> Matrix a
c <*> m = map vp m
  where
    vp = map (c * )

type Vector a = [a]
type Matrix a = [Vector a]

v2c :: Integral a => Vector a -> Vector (Complex Double)
v2c = map (\i -> fromIntegral i :+ 0.0)

m2c :: Integral a => Matrix a -> Matrix (Complex Double)
m2c = map v2c

qbtZero :: Vector (Complex Double)
qbtZero = v2c [1,0]

qbtOne :: Vector (Complex Double)
qbtOne = v2c [0,1]

gateX :: Matrix (Complex Double)
gateX = [qbtOne, qbtZero]

gateH :: Matrix (Complex Double)
gateH = ((1/sqrt 2) :+ 0.0) <*> (m2c [ [1,  1], [1, -1] ])
