module Daimyo.Patterns.Palindrome (
  makePalindrome
) where

-- | makePalindrome
--
-- >>> makePalindrome "abc"
-- "aba"
--
-- >>> makePalindrome "abcba"
-- "abcba"
--
-- >>> makePalindrome "eeeeaaaa"
-- "aaaaaaaa"
--
makePalindrome :: Ord t => [t] -> [t]
makePalindrome s = go s (reverse s)
  where
    go _ [] = []
    go (x:xs) (y:ys) = diff x y : go xs ys
    diff x y
      | x >= y = y
      | x < y  = x
