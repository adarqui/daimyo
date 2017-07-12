import           Data.Char
import           Data.List
import           GHC.Arr

-- | main
--
main :: IO ()
main = do
  msg <- getLine
  let msg_length = length msg
  putStrLn $ solution msg

-- | solution
--
-- >>> solution "aaabbccc"
-- "abc"
--
solution :: String -> String
solution s = reverse $ go s stringArray []
  where
    go [] arr acc               = acc
    go (x:xs) arr acc
      | arr ! (intFromChar x)   = go xs arr acc
      | otherwise               = go xs (arr // [(intFromChar x, True)]) (x:acc)

-- | stringArray
--
stringArray :: Array Int Bool
stringArray = array (0, 26) $ zip [0..26] (repeat False)

-- | intFromChar
--
-- >>> intFromChar 'a'
-- 19
--
intFromChar :: Char -> Int
intFromChar c = (ord $ toLower c) `mod` 26
