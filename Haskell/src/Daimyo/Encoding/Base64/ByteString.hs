-- | Base 64 Encoding.
-- A port, with slight variations, of the C version found here:
-- http://rosettacode.org/wiki/Base64#C (manual implementation)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Daimyo.Encoding.Base64.ByteString (
  alphaTable,
  b64Encode,
  b64EncodePretty,
  b64Decode
) where

import           Data.Bits
import           Data.Char
import           Data.List

import qualified Data.ByteString.Char8 as C

-- | alphaTable: Our base64 lookup table.
alphaTable :: C.ByteString
alphaTable = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- | b64Encode: Simple base64 encode function operating on normal [C.ByteString] strings
--
-- >>> b64Encode "h"
-- "aA=="
--
-- >>> b64Encode "hello\nworld!\n"
-- "aGVsbG8Kd29ybGQhCg=="
b64Encode :: C.ByteString -> C.ByteString
b64Encode stream =
  case C.null stream of
    True -> C.empty
    _ ->
      alphaTable `C.index` (shiftR u' 18) `C.cons`
      alphaTable `C.index` ((shiftR u' 12) .&. 63) `C.cons`
      (if C.length chunk < 2 then '=' else alphaTable `C.index` ((shiftR u' 6) .&. 63)) `C.cons`
      (if C.length chunk < 3 then '=' else alphaTable `C.index` (u' .&. 63)) `C.cons`
      b64Encode (C.drop 3 stream)
  where
    chunk = C.take 3 stream
    u' = u chunk

-- | b64EncodePretty: Intersperses \n every 76 bytes for prettier output
--
-- >>> b64EncodePretty "h"
-- "aA==\n"
b64EncodePretty :: C.ByteString -> C.ByteString
b64EncodePretty = makePretty 76 . b64Encode

-- | u: base64 encoding magic
-- Must be a chunk of at least 3 elements. The rest are discarded.
--
-- >>> u "h\0\0\0"
-- 6815744
--
-- >>> u "hey"
-- 6841721
u :: C.ByteString -> Int
u chunk = fromIntegral result :: Int
  where result = foldl' (.|.) 0 $ map (uncurry shiftL) $ zip (C.foldr (\c acc -> charToInteger c : acc) [] chunk) [16, 8, 0]

-- | charToInteger: Convert a Char to an Integer
charToInteger :: Char -> Integer
charToInteger c = fromIntegral (ord c) :: Integer

-- | makePretty: Add new line characters throughout a character stream
--
-- >>> makePretty 76 "h"
-- "h\n"
makePretty :: Int -> C.ByteString -> C.ByteString
makePretty _ (C.uncons -> Nothing) = C.empty
makePretty by stream = first `C.append` "\n" `C.append` makePretty by rest
  where (first, rest) = C.splitAt by stream

-- | b64Decode: Not implemented yet.
b64Decode :: C.ByteString -> C.ByteString
b64Decode = undefined
