module Daimyo.Text.Search (
  findAllOccurrences,
  splitOnce,
  doubleSplit
) where

import           Data.Text (Text)
import qualified Data.Text as T



-- | find all occurrences of s1 in s2
-- | true indicates a match
-- | false indicates no match
--
-- >>> findAllOccurrences "one" "one two one three one one"
-- [0,8,18,22]
--
-- >>> findAllOccurrences "one" "two three four"
-- []
--
findAllOccurrences :: Text -> Text -> [Int]
findAllOccurrences s1 s2 =
  case (T.null s1, T.null s2) of
    (True, _) -> []
    (_, True) -> []
    _         -> go 0 [] s2
  where
  s1_len        = T.length s1
  go n acc s
    | T.null s  = reverse acc
    | otherwise = if T.isPrefixOf s1 s
                     then go (n+s1_len) (n : acc) (T.drop s1_len s)
                     else go (n+1)      acc       (T.drop 1 s)



-- | split a string once at some substring
--
-- >>> splitOnce "is the" "functional programming is the best"
-- Just ("functional programming ", " best")
--
splitOnce :: Text -> Text -> Maybe (Text, Text)
splitOnce sub s =
  case (findAllOccurrences sub s) of
    [idx] -> Just (T.take idx s, T.drop (T.length sub) $ T.drop idx s) -- Just $ T.drop (T.length sub) $ T.splitAt idx s
    []    -> Nothing



-- | split on c within sub, of s
--
-- >>> doubleSplit " " "is the" "funtional programming is the best"
--
doubleSplit :: Text -> Text -> Text -> Maybe (Text, Text)
doubleSplit c sub s = do
  (a, b)   <- splitOnce sub s
  (a', b') <- splitOnce c sub
  return $ (T.concat [a, a'], T.concat [b', b])
