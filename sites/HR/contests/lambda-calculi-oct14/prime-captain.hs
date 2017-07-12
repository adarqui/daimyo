{-# OPTIONS -O2 #-}
import Data.List
import Data.Maybe

data Fate = CENTRAL | LEFT | RIGHT | DEAD deriving (Show, Read)

main :: IO ()
main = do
 _T <- readLn :: IO Int
 case (1 <= _T && _T <= 50) of
  True -> do
   ids'strings <- mapM (\_ -> getLine) [1.._T]
   let results = map (\id -> fate id) ids'strings
   mapM_ (\r -> putStrLn $ show r) results
  _ -> error "Constraint error: 1 <= T <= 50"

fate s = case (fate'maybe s) of
 Nothing -> DEAD
 (Just v) -> v

fate'maybe s = do
 _ <- no'zeros s
 let id = (read s :: Int)
 _ <- id'constraint id
 _ <- isPrime'Maybe id
 case (isPrime'List $ reverse $ lr s, isPrime'List $ reverse $ rl s) of
  (True, True) -> Just CENTRAL
  (True, False) -> Just LEFT
  (False, True) -> Just RIGHT
  (False, False) -> Just DEAD

no'zeros s = if (any (=='0') s) then Nothing else (Just True)

id'constraint id = if (1 <= id && id <= (10^6)) then (Just True) else Nothing

lr [] = []
lr s = (read s :: Int) : lr (tail s)

rl [] = []
rl s = (read s :: Int) : rl (init s)

isPrime'List [] = False
isPrime'List l = all isPrime l

isPrime'Maybe n = case (isPrime n) of
 True -> Just True
 False -> Nothing

-- ripped
isPrime n = n > 1 &&
 foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
 True primes
 
primes = 2 : filter isPrime [3,5..]

primeFactors n | n > 1 = go n primes
 where
     go n ps@(p:t)
        | p*p > n    = [n]
        | r == 0     =  p : go q ps
        | otherwise  =      go n t
                where
                  (q,r) = quotRem n p
