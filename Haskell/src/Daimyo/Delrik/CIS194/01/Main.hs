module Main 
(
toDigits,
toDigitsRev,
main
)
where

toDigits,toDigitsRev :: Integer -> [Integer]

toDigits a
  | a < 1 = []
  | otherwise = toDigits' a

toDigits'  = map (read . (:[])) . show

toDigitsRev = reverse . toDigits

main :: IO ()
main = putStrLn $ (show . toDigits) 12345678
