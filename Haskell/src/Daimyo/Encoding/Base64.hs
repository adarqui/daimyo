-- | Base 64 Encoding.
-- A port, with slight variations, of the C version found here:
-- http://rosettacode.org/wiki/Base64#C (manual implementation)
module Daimyo.Encoding.Base64 (
  alphaTable,
  b64Encode,
  b64EncodePretty,
  b64Decode
) where

import           Data.Bits
import           Data.Char
import           Data.List

-- | alphaTable: Our base64 lookup table.
alphaTable :: [Char]
alphaTable = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- | b64Encode: Simple base64 encode function operating on normal [Char] strings
--
-- >>> b64Encode "h"
-- "aA=="
--
-- >>> b64Encode "hello\nworld!\n"
-- "aGVsbG8Kd29ybGQhCg=="
b64Encode :: [Char] -> [Char]
b64Encode stream =
  case stream of
    [] -> []
    _ ->
      alphaTable !! shiftR u' 18 :
      alphaTable !! (shiftR u' 12 .&. 63) :
      (if length chunk < 2 then '=' else alphaTable !! ((shiftR u' 6) .&. 63)) :
      (if length chunk < 3 then '=' else alphaTable !! (u' .&. 63)) :
      b64Encode (drop 3 stream)
  where
    chunk = take 3 stream
    u' = u chunk

-- | b64EncodePretty: Intersperses \n every 76 bytes for prettier output
--
-- >>> b64EncodePretty "h"
-- "aA==\n"
b64EncodePretty :: [Char] -> [Char]
b64EncodePretty = makePretty 76 . b64Encode

-- | u: base64 encoding magic
-- Must be a chunk of at least 3 elements. The rest are discarded.
--
-- >>> u "h\0\0\0"
-- 6815744
--
-- >>> u "hey"
-- 6841721
u :: [Char] -> Int
u chunk = fromIntegral result :: Int
  where result = foldl' (.|.) 0 $ map (uncurry shiftL) $ zip (map charToInteger chunk) [16, 8, 0]

-- | charToInteger: Convert a Char into an Integer
charToInteger :: Char -> Integer
charToInteger c = fromIntegral (ord c) :: Integer

-- | makePretty: Add new line characters throughout a character stream
--
-- >>> makePretty 76 "h"
-- "h\n"
makePretty :: Int -> [Char] -> [Char]
makePretty _ [] = []
makePretty by stream = first ++ "\n" ++ makePretty by rest
  where (first, rest) = splitAt by stream

-- | b64Decode: Not implemented yet.
b64Decode :: [Char] -> [Char]
b64Decode = undefined
