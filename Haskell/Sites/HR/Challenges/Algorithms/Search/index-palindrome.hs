{-# OPTIONS -O2 #-}
import Control.Monad

main :: IO ()
main = do
 _T <- readLn :: IO Int
 strings <- replicateM _T getLine
 mapM_ (putStrLn . show . findPalindrome) strings

findPalindrome s@(sh:st) = findPalindrome' 0 s
findPalindrome' index [] = (index-1)
findPalindrome' index b@(bh:bt)
    | isPalindrome b = (index-1)
    | otherwise = findPalindrome' (index+1) bt

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome [x] = False
isPalindrome l = isPalindrome' l (reverse l)

isPalindrome' [] [] = True
isPalindrome' (x:xs) (y:ys)
 | x == y = isPalindrome' xs ys
 | x /= y = False
