{-# OPTIONS -O2 #-}
import           Control.Monad
import           Data.Bits
import           Data.List

main :: IO ()
main = do
 _T <- readLn :: IO Integer
 pairs <- mapM (\_ -> getPair) [1.._T]
 mapM_ (\(n, nums) -> putStrLn $ show $ solve nums) pairs

solve nums = (orsum nums * 2^(length nums - 1)) `mod` modp

modp = ((10^9) + 7)

orsum [] = 0
orsum (x:xs) = x .|. orsum xs

xorsum [] = 0
xorsum (x:xs) = x `xor` xorsum xs

getPair :: IO (Integer,[Integer])
getPair = do
 n <- readLn :: IO Integer
 nums <- getLine >>= \line -> return $ map (\n -> read n :: Integer) $ words line
 return (n, nums)

subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]
