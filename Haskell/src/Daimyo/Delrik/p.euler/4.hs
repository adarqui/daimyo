module 
    PEuler4(palindromes) 
where


-- plaindrom
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.
--
-- poc:
-- filter (((==) 0 ) . ( `mod` 2) . length . snd) $ map (\x -> ((length . show) x, show x)) $ zipWith (*) [100..999] [100..999]
palindromes :: [Int]
palindromes = 
    map (read . snd) $ filter (\(ln,pdc) -> isPalindrome pdc) $ filter toEvenLengthPalindromes $ map vectorize $ [x * y | x <- [100..999], y <- [100..999]]
    where
        toEvenLengthPalindromes :: (Int,String) -> Bool
        toEvenLengthPalindromes = ( ((==) 0 ) . ( `mod` 2) . length . snd)

        vectorize :: Int -> (Int, String)
        vectorize x = 
            let 
                sx = show x 
            in (length sx, sx)
-- abc xyz
-- 900 009
--
isPalindrome :: String -> Bool
isPalindrome (a:b:c:x:y:z:[])
    | reverse [a,b,c] == [x,y,z]  = True
    | otherwise =  False
--isPalindrome' :: Int -> Bool
--isPalindrome' i = x ++ y == i
--    where
--        x 
--        width

largestPalindrome :: Int
largestPalindrome = (head . reverse) palindromes
