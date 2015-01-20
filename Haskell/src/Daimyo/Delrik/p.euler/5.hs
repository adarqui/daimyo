module Main (
    euler'p2,
    isPrime,
    primes,
    pf,
    isInt,
    smallestPrimeFactor,
    isFactor,
    pf2,
    baseMultiple,
    main
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
isPrime n = not $ any (isFactor n) (possibles n)

possibles :: Float -> [Float]
possibles n = takeWhile (\x -> x*x <= n) [2..n]

isFactor :: Float -> Float -> Bool
isFactor x n = (round  x) `mod` (round n) == 0

isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

primes :: [Float]
primes = 1 : 2 : filter isPrime [3..]

smallestPrimeFactor :: Float -> [Float]
smallestPrimeFactor x = filter (\primeF -> isInt (x/primeF) 10) $ tail primes


pf2 :: Float -> ([Float], Float)
pf2 n = pf2' ([n],0)
    where
        pf2' :: ([Float],Float) -> ([Float], Float)

        pf2' (hd:hds,b)
            | isPrime hd = (hd:hds, b)

        pf2' (hd:hds,b) = do
            pf2' (hd':divisor:hds, succ b)
            where
                divisor = head $ smallestPrimeFactor hd
                hd' = hd / divisor

pf :: Float -> IO ([Float], Float)
pf 2 = return ([1,2], 2)
pf 1 = return ([1,1], 1)
pf n = pf' ([n], 0)
    where 
        pf' :: ([Float],Float) -> IO ([Float], Float)

        pf' ((n:ns), m)
            | isPrime n = return (n:ns, succ m)

        pf' (hd:ns,b) = do
            let 
                smallestPrime = takeWhile (\x -> do isInt(hd/x) 20) $ tail primes
                in do
                    putStrLn (">: hd:" ++ (show hd) ++ "| it: " ++ show b ++ "| prime: " ++ show smallestPrime)
                    pf' ((hd / head smallestPrime):(head smallestPrime):ns,succ b)

        isMultiple :: Float -> Float -> Bool
        isMultiple m n'' = ((round m) `mod` (round n)) == 0

        isEvenlyDivisible x' = x' == fromInteger (round x') 

baseMultiple :: Float -> Float
baseMultiple x = product $ nub $ concatMap fst $ map pf2 [1..x]

main :: IO ()
main = putStrLn $ (show.round) $ head $ filter (\x -> all (isFactor x) [1..23]) $ map ((baseMultiple 23)*) [1..]
