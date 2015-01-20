module Main
( main ,
deleteLineBreaks,
thirteenAdjacentDigitMap,
zeroThruNine,
putStrPossibility,
containsZeros,
calc,
matchup
)
where

thirteenAdjacentDigitMap :: String -> [String] -> [String]
thirteenAdjacentDigitMap "" b = reverse $ filter (\str -> (length str) > 12) b
thirteenAdjacentDigitMap str@(s:ss) b =
    thirteenAdjacentDigitMap ss (chunk:b)
    where
        chunk = take 13 str

deleteLineBreaks :: String -> String
deleteLineBreaks = filter (\x -> x `elem` zeroThruNine )

zeroThruNine :: String
zeroThruNine = concat $ map show [0..9]

putStrPossibility :: [String] -> IO ([String])
putStrPossibility [] = return ([])
putStrPossibility (p:ps) = do
    putStrLn $ p ++ "," ++ (show . calc) p
    putStrPossibility ps

containsZeros :: String -> Bool
containsZeros s = any (\c -> c == '0') s

calc :: String -> Integer
calc s = product $ map (\x -> read [x] :: Integer) s

matchup :: (String,Integer) -> (String, Integer) -> (String, Integer)
matchup champ challenger = if champScore > challengerScore then (fst champ, champScore) else (fst challenger, challengerScore)
    where
        champScore = (calc . fst)  champ
        challengerScore = (calc . fst) challenger

main :: IO ()
main = do
    readFile "numbers.block" >>= \rawContents ->
        let
            possibles = thirteenAdjacentDigitMap (deleteLineBreaks rawContents) []
            firstPass = filter (not . containsZeros) possibles
            secondPass = map (\s -> (s,0)) firstPass
            highest = foldl1 matchup secondPass
        in
            putStrLn $ show highest
