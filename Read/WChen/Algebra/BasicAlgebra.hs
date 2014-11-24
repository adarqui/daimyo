module WChen.Algebra.BasicAlgebra (
) where

{- source: http://rutherglen.science.mq.edu.au/~maths/notes/wchen/lnemfolder/em01.pdf -}

import Data.List
import Data.Ratio
import GHC.Real

_N = [1..]

_Z = [(minBound :: Int)..(maxBound :: Int)]

_Q = [ (fromIntegral p :: Float)/(fromIntegral q :: Float) | p <- _Z, q <- _N ]

fakePi = 22/7

{- Order of operations -}

e121 = -3 * 4 - 5 + (-3)
e122 = 21 + 32 `div` (-4) + (-6)
e123 = (366 `div` (-6) - (-6)) `div` (-11)
e124 = 720 `div` (-9) `div` 4 * (-2)
e125 = (76 / 2 - (-2) * 9 + 4 * 8) / 4 / 2 - (10 - 3 * 3) - 6

e126 = sqrt e125
e127 = sqrt 27

{- Distributive Laws -}
p0'a a b c = a * (b + c) == (a*b) + (a*c)
p0'b a b c = (a + b) * c == (a*c) + (b*c)
p0'c a b c d = (a + b)*(c + d) == (a*c) + (a*d) + (b*c) + (b*d)


{- Laws on Squares -}

-- proof: (a + b)^2 = a^2 + 2ab + b^2
p1'0 a b = (a + b) ** 2
p1'1 a b = (a + b) * (a + b)
p1'2 a b = (a**2) + (a*b) + (b*a) + (b**2)
p1'3 a b = (a**2) + 2*(a*b) + (b**2)
p1'proof a b = 1 == (length $ nub $ map (\f -> f a b) [p1'0,p1'1,p1'2,p1'3])

-- proof: (a - b)^2 = a^2 - 2ab + b^2
p2'0 a b = (a - b) ** 2
p2'1 a b = (a - b) * (a - b)
p2'2 a b = (a**2) + (a*(-b)) + ((-b)*a) + ((-b)**2)
p2'3 a b = (a**2) - 2*(a*b) + (b**2)
p2'proof a b = 1 == (length $ nub $ map (\f -> f a b) [p2'0,p2'1,p2'2,p2'3])

-- proof: (a - b)(a + b) = a^2 - b^2
p3'0 a b = (a - b)*(a + b)
p3'1 a b = (a**2) + (a*b) - (b*a) - (b**2)
p3'2 a b = (a**2) - (b**2)
p3'proof a b = 1 == (length $ nub $ map (\f -> f a b) [p3'0,p3'1,p3'2])


{- Laws on cubes -}
p4'0 a b = (a - b) * ((a**2) + (a*b) + (b**2))
p4'1 a b = (a**3) + ((a**2)*b) + (a*(b**2)) - (b*(a**2)) - (a*(b**2)) - (b**3)
p4'2 a b = (a**3) - (b**3)
p4'proof a b = 1 == (length $ nub $ map (\f -> f a b) [p4'0,p4'1,p4'2])


{- solve: (2x + 5)^2 - (x + 5)^2 -}
{- can't just plug in if there's a coefficient to x etc -}
ex131'0 x = a - b
    where
        a = ((2*x) + 5)**2
        b = (x+5)**2
ex131'1 x = a - b
    where
        a = (2*x + 5)*(2*x + 5)
        b = (x + 5)*(x + 5)
ex131'2 x = a - b
    where
        a = ((4*x)**2) + (10*x) + (10*x) + 25
        b = (x**2) + (5*x) + (5*x) + 25
ex131'3 x = a - b
    where
        a = ((4*x)**2) + (20*x) + 25
        b = (x**2) + (10*x) + 25
ex131'4 x = ((3*x)**2) + (10*x)
ex131'proof x = 1 == (length $ nub $ map (\f -> f x) [ex131'0,ex131'1,ex131'2,ex131'4])

{- broken down ex131 -}
-- ((2*2)+5)*((2*2)+5) = (4+5)(4+5) = 9*9
-- ((4*2)^2) + (20*2) + 25 = (8^2) + (40) + 25 = 64 + 40 + 25 = 129
ex131'a'0 x = ((2*x) + 5)**2
ex131'a'1 x = ((2*x) + 5)*((2*x) + 5)
ex131'a'2 x = ((4*x)**2) + (10*x) + (10*x) + 25
ex131'a'3 x = ((4*x)**2) + (20*x) + 25
ex131'a'proof x = 1 == (length $ nub $ map (\f -> f x) [ex131'a'0,ex131'a'1,ex131'a'2,ex131'a'3])

ex131'b'0 x = ((x+5)**2)
ex131'b'1 x = (x+5)*(x+5)
ex131'b'2 x = (x**2) + (5*x) + (5*x) + 25
ex131'b'3 x = (x**2) + (10*x) + 25
ex131'b'proof x = 1 == (length $ nub $ map (\f -> f x) [ex131'b'0,ex131'b'1,ex131'b'2,ex131'b'3])


{- solve: (x - y)(x + y - 2) + 2x -}
ex132'0 x y = (x - y) * (x + y - 2) + (2*x)
ex132'1 x y = (x**2) + (x*y) - (2*x) - (x*y) - (y**2) + (2*y) + (2*x)
ex132'2 x y = (x**2) - (2*x) - (y**2) + (2*y) + (2*x)
ex132'3 x y = (x**2) - (y**2) + (2*y)
ex132'proof x y = 1 == (length $ nub $ map (\f -> f x y) [ex132'0,ex132'1,ex132'2,ex132'3])

{- solve: (x + 1)(x - 2)(x + 3) -}
ex133'0 x = ((x**2) - (2*x) + x - 2) * (x + 3)
ex133'1 x = ((x**2) - x - 2) * (x + 3)
ex133'2 x = (x**3) + (3*(x**2)) - (x**2) - (3*x) - (2*x) - 6
ex133'3 x = (x**3) + (2*(x**2)) - (5*x) - 6
ex133'proof x = 1 == (length $ nub $ map (\f -> f x) [ex133'0,ex133'1,ex133'2,ex133'3])

{- solve: (5x + 3)^2 - (2x - 3)^2 + (3x - 2)(3x + 2) -}
ex134'0 x = a - b + c
    where
        a = (25*(x**2)) + (2*(5*x*3)) + 9   -- a^2 + 2ab + b^2
        b = (4*(x**2)) + (2*(2*x)*(-3)) + 9 -- a^2 + 2ab + b^2
        c = (9*(x**2)) - 4                  -- a^2 - b
ex134'1 x = a - b + c
    where
        a = (25*(x**2)) + (30*x) + 9
        b = (4*(x**2)) - (12*x) + 9
        c = (9*(x**2)) - 4
ex134'2 x = (25*(x**2)) - (4*(x**2)) + (9*(x**2)) + (42*x) - 4
ex134'proof x = 1 == (length $ nub $ map (\f -> f x) [ex134'0,ex134'1,ex134'2])


{- Arithmetic of Fractions -}

pFracAdd (a :% b) (c :% d) = (a*d + b*c) / (b*d)
pFracSub (a :% b) (c :% d) = (a*d - b*c) / (b*d)

{- 1/3 + 1/6 -}
ex141 = (1/3) + (1/6)
ex141'b = pFracAdd (1 :% 3) (1 :% 6)

ex141'0 = (1/3) + (1/6)
ex141'1 = (2/6) + (1/6)
ex141'2 = (2+1)/6
ex141'3 = (3/6)
ex141'4 = (1/2)

ex142'0 x = a - b
    where
        a = ((x - 4)**2) / ((x + 4)**2)
        b = ((x + 2)**2) / ((x + 4)**2)

ex142'1 x = (((x - 4)**2) - ((x - 2)**2)) / ((x + 4)**2)

ex142'2 x = (a - b) / ((x + 4)**2)
    where
        a = (x**2) - (2*x*(-4)) + ((-4)**2)
        b = (x**2) - (2*x*(-2)) + ((-2)**2)

ex142'3 x = (a - b) / ((x + 4)**2)
    where
        a = (x**2) + (8*x) + 16
        b = (x**2) + (4*x) + 4

ex142'4 x = ((12*x) - 12) / ((x + 4)**2)


ex144'0 x y = (x / y) - (x / (x + y))
ex144'1 x y = (numA - numB) / denom
    where
        numA = (x * (x + y))
        numB = (x * y)
        denom = (y * (x + y))

ex144'2 x y = (numA - numB) / denom
    where
        numA = (x**2) + (x*y)
        numB = (x*y)
        denom = (y * (x + y))

ex144'3 x y = (x**2) / (y * (x + y))



{- solve: p/(p-q) + q/(q-p) -}

ex146'0 p q = a + b
    where
        a = p/(p-q)
        b = q/(q-p)

ex146'1 p q = a + b
    where
        a = p/(p-q)
        b = (-q)/(p-q)

ex146'2 p q = (p + (-q)) / (p - q)

ex146'3 p q = (p - q) / (p - q)

ex146'4 p q = 1
