module Daimyo.Lib.Patterns.Palindrome (
 makePalindrome,
 makePalindromeX
) where

import Data.Char
import Control.Monad

main :: IO ()
main = do
 _T <- readLn :: IO Int
 strings <- replicateM _T getLine
 mapM_ (putStrLn . show . makePalindrome) strings

makePalindrome s = makePalindrome' s (reverse s)

makePalindrome' _ [] = 0
makePalindrome' s@(sx:ss) rs@(rsx:rss) = (diff sx rsx) + makePalindrome' ss rss

diff sc rc
 | sc >= rc = 0
 | sc < rc = (ord rc - ord sc)

makePalindromeX s = makePalindromeX' s (reverse s)

makePalindromeX' _ [] = []
makePalindromeX' s@(sx:ss) rs@(rsx:rss) = (diffX sx rsx) : makePalindromeX' ss rss

diffX sc rc
 | sc >= rc = rc
 | sc < rc = sc

t1 = makePalindrome "abc"
t2 = makePalindrome "abcba"
t3 = makePalindrome "abcd"
t4 = makePalindrome "eeeeaaaa"
t5 = makePalindrome "eeeeeeea"
