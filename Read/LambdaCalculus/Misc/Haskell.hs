module LambdaCalculus.Misc.Haskell (
) where

e1 = \y -> y
e2 = \y -> \x -> y*x

_I = \x -> x
_K = \x y -> x
_K' = \x y -> y
_S = \x y z -> x z (y z)

__I m = m
__K m n = m
__K' m n = n
__S m n l = m l (n l)

t1 = _S (+) id 3

true = _K
false = _K'

t2 = \a b c -> b (a b c)
t2' = \a -> \b -> \c -> b (a b c)
t2'' a b c = b (a b c)
t2''' :: ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
t2''' a b c = b (a b c)

ex2_t2 = t2''' id (+1) 1
ex3_t2 c = (\x -> x + 1) ((\x -> x) (\x -> (x + 1)) c)

{-
    t2'''              a                     b             c
    t2''' ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
    t2''' ((\x -> x) -> Int -> Int) -> (\x -> x + 1) -> 1 -> Int
-}


data ChurchNumeral = Z | S ChurchNumeral deriving (Show)

s Z = 0
s (S n) = 1 + s n

(s') = \n s z -> s (n s z)
(add') = \a b -> b (s') a

ack (0, b) = b + 1
ack (a, 0) = ack (a-1, 1)
ack (a, b) = ack (a-1, ack (a,b-1))

data P = P !Int !Int
ack' :: P -> (Int -> Int) -> Int
ack' (P 0 n) k = k (n + 1)
ack' (P m 0) k = ack' (P (m-1) 1) k
ack' (P m n) k = ack' (P m (n-1)) (\a -> ack' (P (m-1) a) k)


t3 = (\x -> 3 * x) 4
t4 = (\y -> y 5) (\x -> 3 * x)
t4' = (\x -> 3 * x) 5
--t5 = (\x -> x x) (\x -> x x) -> no normal form

q = \x -> x * x
p8 = \x -> q (q (q x))
t f = (\x -> f (f (f x)))
