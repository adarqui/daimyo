module Daimyo.Random (
  getBytes,
  getString,
  getRaw,
  getAscii,
  getAlpha,
  getLower,
  getUpper,
  getDigits,
  getBits,
  getWords,
  get8,
  get16,
  get32,
  get64,
  getMix,
  getX,
  getPair,
  getPairNe,
  w64ToInt,
  intToW64,
  listOfWord64,
  listOfWord
) where

import           Daimyo.Bits

import           Data.Char
import           Data.Word

import           System.IO
import           System.IO.Unsafe

import qualified Data.Binary.Get       as Bin
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Data.ByteString (ByteString)

-- | getBytes
--
-- returns a random ByteString of length 'n', using /dev/urandom
--
getBytes :: Int -> ByteString
getBytes n = unsafePerformIO $
  withFile "/dev/urandom" ReadMode $ (flip BC.hGet n)

-- | getString
--
-- returns a random ByteString that is sanitized via the 'filt' filters.
--
getString :: [Char -> Bool] -> Int -> [Char]
getString filt = filter (\c -> and (map (\f -> f c) filt)) . BC.unpack . getBytes

-- | getRaw
--
getRaw :: Int -> [Char]
getRaw = getString [const True]

-- | getAscii
--
getAscii :: Int -> [Char]
getAscii = getString [isAscii]

-- | getAlpha
--
getAlpha :: Int -> [Char]
getAlpha = getString [isAscii, isAlpha]

-- | getLower
--
getLower :: Int -> [Char]
getLower = getString [isAscii, isLower]

-- | getUpper
--
getUpper :: Int -> [Char]
getUpper = getString [isAscii, isUpper]

-- | getDigits
--
getDigits :: Int -> [Char]
getDigits = getString [isAscii, isDigit]

-- | getBits
--
getBits :: Int -> [Int]
getBits = concatMap (charToBits . ord) . getRaw

-- | listOfWord64
--
listOfWord64 :: Bin.Get [Word64]
listOfWord64 = do
  empty <- Bin.isEmpty
  if empty
    then return []
    else do
      v <- Bin.getWord64be
      rest <- listOfWord64
      return (v : rest)

-- | listOfWord
--
listOfWord :: Bin.Get t -> Bin.Get [t]
listOfWord gw = do
  empty <- Bin.isEmpty
  if empty
    then return []
    else do
      v <- gw
      rest <- listOfWord gw
      return (v : rest)

-- | getWords
--
getWords :: Int -> [Word64]
getWords n =
    unsafePerformIO $ do
        v' <- withFile "/dev/urandom" ReadMode $
            (\h -> do
                v <- BL.hGet h (n*8)
                return v
            )
        return (Bin.runGet listOfWord64 v')

-- | get8
--
get8 :: Int -> [Word64]
get8 = getX 8

-- | get16
--
get16 :: Int -> [Word64]
get16 = getX 16

-- | get32
--
get32 :: Int -> [Word64]
get32 = getX 32

-- | get64
--
get64 :: Int -> [Word64]
get64 = getX 63

-- | getMix
--
getMix :: Int -> [Word64]
getMix n = concat $ [get8 n, get16 n, get32 n, get64 n]

-- | getX
--
getX :: Word64 -> Int -> [Word64]
getX lim = map (`mod` lim) . getWords

getPair :: Word64 -> (Word64, Word64)
getPair lim = (x,y)
  where (x:y:[]) = getX lim 2

-- | getPairNe
--
-- get a pair of unequal integers
--
getPairNe :: Word64 -> (Word64, Word64)
getPairNe lim = if (x == y) then getPairNe lim else pair
  where pair@(x,y) = getPair lim

-- | w64ToInt
--
w64ToInt :: Integral a => a -> Int
w64ToInt = fromIntegral

-- | intToW64
--
intToW64 :: Integral a => a -> Word64
intToW64 = fromIntegral
