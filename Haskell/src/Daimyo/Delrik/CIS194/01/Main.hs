module Main 
(
toDigits,
main
)
where

toDigits,toDigitsRev :: Integer -> [Integer]

toDigits 0 = []
toDigits n = map (read . (:[])) $ show n

toDigitsRev = reverse . toDigits

main :: IO ()
main = putStrLn "Excercise 1"
