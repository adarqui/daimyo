{-
    sources:

        Primes
            http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
            https://www.haskell.org/haskellwiki/Prime_numbers

        Primality Test
            http://en.wikipedia.org/wiki/Primality_test
            http://en.wikipedia.org/wiki/AKS_primality_test
-}

module Daimyo.NumberTheory.Prime (
    primes,
    sieve,
    sieve'init,
    coprimes,
    primes'pairs,
    primes'trial,
    primeFactors,
    primeMultiples,
    primeMultiples'products,
    isPrime,
    primes',
    isFactor,
    nth'sieve,
    relatively'prime,
    prime'number'theorem,
    t_prime'number'theorem,
    prime'number'theorem'approx,
    t_prime'number'theorem'approx,
    limit'prime'number'theorem,
    primeFactors',
    primeFactors'Explain,
    t_primeFactors'Explain
) where

import Daimyo.Print
import Daimyo.Calculus.Limit
import Daimyo.List.Misc

import Data.List
import Control.Monad
import Control.Monad.Writer
import Text.Printf

{-
    sieve
-}

primes = 2 : sieve 3 [5,7..]

sieve p (x:xs) =
    let
        xs' = [ x' | x' <- xs, x' `mod` p > 0]
    in
        p : sieve x xs'

{-
    attempt to start the sieve at a specific prime
-}

sieve'init p =
    let
        (p1:rest) = [ x | x <- [p,p+1..], isPrime x]
    in
        sieve p1 rest


{-
    sieve
-}

coprimes = cosieve 4 [6..]

cosieve p (x:xs) =
    let
        xs' = [ x' | x' <- xs, not $ isPrime x' ] 
    in
        p : cosieve x xs'

{-
    pairs of primes (p, p+k)
-}
primes'pairs = primes'pairs' primes
primes'pairs' (x:y:xs) = (x,y) : primes'pairs' (y:xs)


{-
    trial division
-}

primes'trial = 2 : primes'trial' 3 [5,7..]
    where
        primes'trial' p rest =
            let
                (p':rest') = filter (\n -> n `mod` p /= 0) rest
            in
                p : primes'trial' p' rest'




{-
    Stolen
    https://www.haskell.org/haskellwiki/99_questions/Solutions/35
-}

primeFactors :: Integer -> [Integer]
primeFactors a = let (f, f1) = factorPairOf a
                     f' = if prime f then [f] else primeFactors f
                     f1' = if prime f1 then [f1] else primeFactors f1
                 in f' ++ f1'
 where
 factorPairOf a = let f = head $ factors a
                  in (f, a `div` f)
 factors a    = filter (isFactor a) [2..a-1]
 isFactor a b = a `mod` b == 0
 prime a      = null $ factors a

isPrime n = n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes

primes' = 2 : filter isPrime [3,5..]

primeFactors' n | n > 1 = go n primes'
 where
     go n ps@(p:t)
        | p*p > n    = [n]
        | r == 0     =  p : go q ps
        | otherwise  =      go n t
                where
                  (q,r) = quotRem n p


primeMultiples n = filter (not . null) $ nub $ subsequences $ primeFactors' n

primeMultiples'products n = map product $ primeMultiples n


isFactor fac n = n `rem` fac == 0

sieve' :: [Integer]
sieve' = sieve' [2..]
  where
    sieve' (p:xs) = p : sieve' [x|x <- xs, x `mod` p > 0]

nth'sieve n = sieve' !! n


{-
    two integers (a,b) are relatively prime if gcd (a, b) == 1
-}

relatively'prime a b = gcd a b == 1


prime'number'theorem :: Integer -> Int
prime'number'theorem x = length $ takeWhile (<= x) primes

t_prime'number'theorem = map prime'number'theorem [100,200..1000]

prime'number'theorem'approx :: Integer -> Double
prime'number'theorem'approx x =
    let
        x' = fromIntegral x :: Double
    in
        x' / (log x' - 1)

t_prime'number'theorem'approx = map prime'number'theorem'approx [100,200..1000]

limit'prime'number'theorem =
    lim
        (\x ->
            (fromIntegral (prime'number'theorem (truncate x)) :: Double) / prime'number'theorem'approx (truncate x))

{-
    primeFactors explain via writerT
-}

primeFactors'Explain :: Integer -> IO ([Integer], [String])
primeFactors'Explain n = do
    (r, w) <- runWriterT (primeFactors'Explain' n)
    return (r, w)

primeFactors'Explain' :: Integer -> WriterT [String] IO [Integer]
primeFactors'Explain' n = do
    tell [printf "primeFactors (n=%d)" n]
    primeFactors'Explain'' 0 n primes'

primeFactors'Explain'' :: Int -> Integer -> [Integer] -> WriterT [String] IO [Integer]
primeFactors'Explain'' sp n ps@(p:t) = do
    tell [indentSp sp $ printf "primeFactors (n=%d) ps@(p=%d:%s)" n p (show $ take 10 t)]
    tell [indentSp sp $ printf "(q=%d, r=%d) = quotRem (n=%d) (p=%d)" q r n p]
    primeFactors'Explain''' sp n ps q r
    where
        (q,r) = quotRem n p

primeFactors'Explain''' :: Int -> Integer -> [Integer] -> Integer -> Integer -> WriterT [String] IO [Integer]
primeFactors'Explain''' sp n ps@(p:t) q r
    | p*p > n    = do
        tell [indentSp sp $ printf "(p=%d)*(p=%d) > (n=%d)" p p n]
        tell [indentSp sp $ printf "(p*p=%d) > (n=%d)" (p*p) n]
        return [n]
    | r == 0     =  do
        tell [indentSp sp "r==0"]
        tell [indentSp sp $ printf "primeFactors (q=%d) ps" q]
        v <- primeFactors'Explain'' (sp+2) q ps
        return (p : v)
    | otherwise  = do
        tell [indentSp sp "otherwise"]
        tell [indentSp sp $ printf "primeFactors (n=%d) t" n]
        primeFactors'Explain'' (sp+2) n t

t_primeFactors'Explain n = do
    (r,w) <- primeFactors'Explain n
    mapM_ putStrLn w
    putStrLn (show r)
