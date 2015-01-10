module 
    PEuler4(palindromes) 
where


-- plaindrom
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.
--
-- poc:
-- filter (((==) 0 ) .( `mod` 2) . length . snd) $ map (\x -> ((length . show) x, show x)) $ zipWith (*) [100..999] [100..999]
palindromes :: [Int]
palindromes = 
    [x*y | x <- [100..999] , y <- [100..999]]

    foldl product 
