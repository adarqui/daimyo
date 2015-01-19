module Daimyo.Delrik (
    euler'p2,
    isPrime,
    primes,
    pf
) where

import Data.List

-- http://owp.ghost.io/project-euler-1/
--
--

euler'p2 :: (Floating a, Integral a) => a -> [(a,a)]
euler'p2 x = takeWhile isPrime $ zip [x,(x-1)..hf] [1..x]
    where
        hf = sqrt x
        op y= (x `mod` y)
        isPrime (x',y') = case (op y') of 
            0 -> False;
            _ -> True

isPrime :: Float -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n = null [x | x <- possibles, (round  n) `mod` (round x) == 0]
    where
        possibles :: [Float]
        possibles = takeWhile (\x -> x*x <= n) [2..n]

primes :: [Float]
primes = 1 : 2 : filter isPrime [3..]
pf :: Float -> IO ([Float], Float)
pf 2 = return ([1,2], 2)
pf 1 = return ([1,1], 1)
pf n = pf' ([n], 0)
    where 
        pf' :: ([Float],Float) -> IO ([Float], Float)

        pf' ((n:ns), m)
            | isPrime n = return (n:ns, succ m)

        pf' (hd:ns,b) = do
            putStrLn (">: hd:" ++ (show hd) ++ "| it: " ++ show b ++ "| prime: " ++ show smallestPrime )
            let n' = hd / smallestPrime in pf' (n':smallestPrime:ns,succ b)

        isMultiple :: Float -> Float -> Bool
        isMultiple m n'' = ((round m) `mod` (round n)) == 0
        smallestPrime = head $ takeWhile (isEvenlyDivisible) $ filter (isMultiple n) $ tail primes
        isEvenlyDivisible x' = x' == fromInteger (round x') 
