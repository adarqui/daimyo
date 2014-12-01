{-# LANGUAGE ParallelListComp, NPlusKPatterns #-}

import Control.Monad

sum'0 n = (n*(n+1))/2
sum'1 n = ((n^2) + n) / 2
sum'2 n = 0.5 * ((n^2) + n)
sum'3 n = (0.5 * n^2) + (0.5*n)
{- n+1 ihop -}
sum'ih'1 n = ((n+1)*(n+2))/2
sum'ih'2 n = ((n^2)+(3*n)+2)/2
sum'ih'3 n = 0.5 * ((n^2) + (3*n) + 2)
sum'ih'4 n = 0.5*(n^2) + (1.5*n) + 1

{-
 1
 22
 333
-}
geomSumOdd n = 
    [ (take row $ repeat '*') ++ (take (h - row) $ repeat '.') | row <- [1..h] | col <- [1..w] ]
    where
        h = n
        w = n

showGeomSumOdd n = mapM_ putStrLn $ geomSumOdd n

geomGauss n = map (\l -> '.' : reverse l) $ geomSumOdd n

showGeomGauss n = mapM_ putStrLn $ geomGauss n


{-
    These routines calculate the length of the geometric representations of sum/sumOdd
-}
sumGeom = length . filter (=='*') . concat . geomGauss
sumGeomOdd = length . filter (== '*') . last . geomSumOdd


expMul a m n = a^(m*n)
expMul' a m n = (a^m)^n

odds = odds' 0
    where odds' n = (n+1) : odds' (n+2)

sumOdds n = n^2

sumExp 1 _ = error "a can't be 1"
sumExp a n = ((a^(n+1))-1)/(a-1)

factorial 0 = 1
factorial (n + 1) = (n + 1) * factorial n

fib 0 = 0
fib 1 = 1
fib (n+2) = fib n + fib (n+1)

fibs 0 = [0]
fibs 1 = [1]
fibs (n+2) = fibs n ++ fibs (n+1)

fibSum n = fib (n+2) - 1

fact_proof =
    5 * (5 - 1) * (5 - 2) * (5 - 3) * (5 - 4) * 1

{- fib proof of 5 -}
fib_proof'5 =
    fib'3 + fib'4
    where
        fib'4 = fib'2 + fib'3
        fib'3 = 1 + fib'2
        fib'2 = 0 + 1

{- fib proof of 10 -}
fib_proof'10 =
    fib'8 + fib'9
    where
        fib'9 = fib'7 + fib'8
        fib'8 = fib'6 + fib'7
        fib'7 = fib'5 + fib'6
        fib'6 = fib'4 + fib'5
        fib'5 = fib'3 + fib'4
        fib'4 = fib'2 + fib'3
        fib'3 = 1 + fib'2
        fib'2 = 0 + 1

{- fib (n+2) - 1 -}
fib_proof'sum'5 =
    result - 1
    where
        result = fib'7 + fib'8
        fib'7 = fib 5
        fib'8 = fib 6

fib_proof'sum'5' =
    result - 1
    where
        result = fib'8 + fib'9
        fib'8 = fib 6
        fib'9 = fib 7
        

data Peano = Z | S Peano deriving (Show, Eq)

equals (S x) (S x') = equals x x'
equals Z Z = True
equals _ _ = False

{-
    (x + y) - x = y
    sub (add x y) x = y
-}
add Z Z = Z
add s Z = s
add s (S x) = add (S s) x

sub Z Z = Z
sub Z _ = Z
sub s Z = s
sub (S x) (S x') = sub x x'

addsub_proof = (base (S (S Z)), ihop (S (S (S Z))) (S Z))
    where
        base y = sub (add Z y) Z == sub y Z && sub y Z == y
{- 
For the inductive case, assume sub (add x y) x = y; the aim is to prove
sub (add (Succ x) y) (Succ x) = y.
-}
        ihop x y = sub (add (S x) y) (S x) == y


{-
For example, it is considerably harder to prove (x+y)−y = x
than to prove (x + y) − x = y.
-}


{-
    add is associative:

        add x (add y z) = add (add x y) z
-}
addassoc_proof =
    (base, ihop (S Z) (S (S Z)) (S (S (S Z))))
    where
        base = add Z (add (S Z) (S Z)) == (S (S Z))
        ihop x y z = add (S x) (add y z) == add (add (S x) y) z


addzero x = add x Z
addzero'ihop x = S x == add (S x) Z

addzero_proof x = (x == add x Z, addzero'ihop x)


addone x y = add (S x) y == add x (S y)
addone_base y = add (S Z) y == S y
addone_ihop x y = add (S x) y == add x (S y)

addcomm_base y = add Z y == add y Z
addcomm_ihop x y = add (S x) y == add x (S y)


{- List induction -}

{- sum (xs ++ ys) == sum xs + sum ys -}
{- this is how i should do proofs -}
sum_proof xs ys = (all (==True) [base, ihops], base, ihops)
    where
        x = head xs
        base = all (==sum ys) [b1,b2,b3]
            where
                b1 = sum ([] ++ ys)
                b2 = 0 + sum ys
                b3 = sum [] + sum ys
        ihops = all (==sum (x:xs++ys)) [ih1,ih2,ih3,ih4]
            where
                ih1 = sum ((x : xs) ++ ys)
                ih2 = x + sum (xs ++ ys)
                ih3 = x + sum xs + sum ys
                ih4 = sum (x : xs) + sum ys
        
{- length (xs ++ ys) = length xs + length ys -}
length_proof xs ys = all (==True) [base, ihops]
    where
        base = all (==length ([]++ys)) [b1,b2]
            where
                b1 = length ([] ++ ys)
                b2 = length ys
        ihops = all (==length (x:xs++ys)) [ih1,ih2,ih3,ih4]
            where
                x = head xs
                ih1 = length ((x:xs) ++ ys)
                ih2 = length [x] + length (xs ++ ys)
                ih3 = length [x] + length xs + length ys
                ih4 = length (x:xs) + length ys


{- length/map proof: length (map f xs) = length xs -}
length_map_proof f xs = all (==True) [base, ihops]
    where
        base = all (==0) [b1,b2]
            where
                b1 = length (map f [])
                b2 = length []
        x = head xs
        ihops = all (==length (x : xs)) [ih1,ih2,ih3,ih4]
            where
                ih1 = length (map f (x : xs))
                ih2 = length (f x : map f xs)
                ih3 = length (map f [x]) + length (map f xs)
                ih4 = length (x:xs)


{- map f (xs ++ ys) = map f xs ++ map f ys -}
map_concat_proof f xs ys = all (==True) [base, ihops]
    where
        base = all (==map f ys) [b1,b2]
            where
                b1 = map f ([] ++ ys)
                b2 = map f [] ++ map f ys
                b3 = [] ++ map f ys
                b4 = map f ys
        x = head xs
        ihops = all (==map f (x:xs++ys)) [ih1,ih2,ih3]
            where
                ih1 = map f ((x:xs)++ys)
                ih2 = f x : map f xs ++ map f ys
                ih3 = map f (x:xs) ++ map f ys

{- map f . map g) xs = map (f.g) xs -}
map_compose_proof f g xs = all (==True) [base, ihops]
    where
        base = all (==map (f.g) []) [b1,b2]
            where
                b1 = (map f . map g) []
                b2 = map (f.g) []
        x = head xs
        ihops = all (==map (f.g) (x:xs)) [ih1,ih2,ih3,ih4]
            where
                ih1 = (map f . map g) (x:xs)
                ih2 = ((f.g) x) : ((map f . map g) xs)
                ih3 = map (f.g) [x] ++ map (f.g) xs
                ih4 = map (f.g) (x:xs)

{- sum (map (1+) xs) = length xs + sum xs -}
sum_map_bleh :: [Int] -> Bool
sum_map_bleh xs = all (==True) [base, ihops]
    where
        base = all (==sum (map (1+) [])) [b1,b2,b3,b4]
            where
                b1 = sum (map (1+) [])
                b2 = sum []
                b3 = 0 + sum []
                b4 = length [] + sum []
        x = head xs
        ihops = all (==sum (map (1+) (x:xs))) [ih1,ih2,ih3,ih4,ih5,ih6]
            where
                ih1 = sum (map (1+) (x:xs))
                ih2 = sum ((1+x) : map (1+) xs)
                ih3 = (1+x) + sum (map (1+) xs)
                ih4 = (1+x) + (length xs + sum xs)
                ih5 = (1 + (length xs)) + (x + (sum xs))
                ih6 = length (x:xs) + sum (x:xs)


{- foldr (:) [] xs = xs -}
foldr_cons_proof :: Eq a => [a] -> Bool
foldr_cons_proof xs = all (==True) [base, ihops]--`, ihops]
    where
        base = all null [b1,b2]
            where
                b1 = foldr (:) [] []
                b2 = []
        x = head xs
        ihops = all (==(x:xs)) [ih1,ih2,ih3]
            where
                ih1 = foldr (:) [] (x:xs)
                ih2 = x : foldr (:) [] xs
                ih3 = x : xs

{- map f (concat xss) = concat (map (map f) xss) -}
map_concat_proof' f xss = all (==True) [base, ihops]
    where
        base = all null [b1,b2,b3,b4]
            where
                b1 = map f (concat [])
                b2 = map f []
                b3 = concat []
                b4 = concat (map (map f) [])
        xs = head xss
        ihops = all (==concat (map (map f) (xs:xss))) [ih1,ih2,ih3,ih4,ih5]
            where
                ih1 = map f (concat (xs : xss))
                ih2 = map f xs ++ map f (concat xss)
                ih3 = map f xs ++ concat (map (map f) xss)
                ih4 = concat (map f xs : map (map f) xss)
                ih5 = concat (map (map f) (xs : xss))


{- foldr (:) [] = id -}
foldr_id_proof xs = all (==True) [base]
    where
        base = all null [b1]
            where
                b1 = foldr (:) [] []
        x = head xs
        ihops = all (==id (x:xs)) [ih1,ih2]
            where
                ih1 = foldr (:) [] (x:xs)
                ih2 = x : foldr (:) [] xs

{- map f . concat = concat (map (map f)) -}
map_compose_concat f xss = all (==True) [base, ihops]
    where
        base = all null [b1]
            where
                b1 = (map f . concat) []
        xs = head xss
        ihops = all (==(map f . concat) (xs:xss)) [ih1, ih2, ih3]
            where
                ih1 = (map f . concat) (xs:xss)
                ih2 = map f xs ++ (map f . concat) xss
                ih3 = map f xs ++ map f (concat xss)
                ih4 = concat (map (map f) (xs:xss))


{- ++ -}
join' :: [a] -> [a] -> [a]
join' [] ys = ys
join' xs [] = xs
join' (x:xs) ys = x : (join' xs ys)

{-
    all horses are the same colour
    forall h in H, P(h)
-}

--th26_horse 

{-
    assoc: a + (b + c) = (a + b) + c
    a : ( b : c) = (a : b) : c
-}


{-
    Exercise 0:

    Prove the sum from i=0 to n of i is (n*(n+1))/2
-}

ex0 = (base, base', ihop 1000)
    where
        base = sum'0 0
        base' = (0*(0+1))/2 == 0 && 0/2 == 0
        ihop n = ((n+1)*((n+1)+1)) / 2 == ((n+1)*(n+2))/2 && ((n+1)*(n+2))/2 == (n^2+(3*n)+2)/2

{-
    Exercise 1:

    Let a be an arbitrary real number. Prove, for all natural numbers m and n, that a^(m*n) = (a^m)^n
-}

ex1 = (base 0 0, base' 0 0, ihop 2 2, ihop' 2 2) 
    where
        a = 10
        base m n = a^(m*n)
        base' m n = (a^m)^n
        ihop m n = a^((m+1)*(n+1))
        ihop' m n = (a^(m+1))^(n+1)

ex1' a m n = a^(m*n) == (a^m)^n
ex1'' a m n = a^((m+1) *( n+1)) == (a^(m+1))^(n+1)

ex1_brute_proof a m n = all (==(True,True)) [ (ex1' a' m' n', ex1'' a' m' n') | a' <- [0..a], m' <- [0..m], n' <- [0..n] ]

ex1_brute_proof' = ex1_brute_proof 1000 10 10


{-
    Exercise 2:

    Prove that the sum of the first n odd positive numbers is n^2
-}

ex2 n = (base, all (==True) $ map ihop (take n odds))
    where
        base = 1^2 == 1
        ihop n = odd $ n^2


{-
    Exercise 3:

    Prove that Sum from i=1 to n of a^i = (a^(n+1)-1)/(a-1), where a is a real number and a != 1
-}

ex3 = (base, base')
    where
        base = (2^1) == sumExp 2 1
        base' = (2^1) == ((2^(1+1)-1))/(2-1)

ex3' = ((2^2)-1)/(1)

{-
    Exercise 4:

    The first few numbers in this famous sequence are 0, 1, 1, 2, 3, 5.... Prove the following:

    Sum from i=1 to n of fib i is equal to fib (n+2)-1
-}

ex4 n = (base, ihop, ihop' n)
    where
        base = fib 0 == 0
        ihop = fib ((n+1)+2) - 1 == fib (n+3) - 1
        ihop' k =
            fib (k+2) - 1 + fib (k+1) == fib (k+3) - 1




{- prove that ++ is associative -}


{-
    Exercise 19:

    Assume that xss is a finite list of type [[a]], that is of length n, and that xs is a finite list and an arbitrary element of xss. Prove that length (concat xss) = sum (map length xss))
-}

ex19 xss = all (==True) [base,ihops]
    where
        base = all (==sum (map length [])) [b1,b2]
            where
                b1 = length (concat [])
                b2 = length []
        xs = head xss
        ihops = all (==sum (map length (xs:xss))) [ih1,ih2,ih3,ih4]
            where
                ih1 = length (concat (xs:xss))
                ih2 = length xs + length (concat xss)
                ih3 = sum (map length [xs]) + sum (map length xss)
                ih4 = sum (map length (xs:xss))


ex26 xs = foldr (&&) True xs
