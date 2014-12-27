module Daimyo.Random (
    getBytes,
    getString,
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
    intToW64
) where

import Daimyo.Bits

import Data.Char
import Data.Bits
import Data.Word
import GHC.Word

import System.IO
import System.IO.Unsafe

import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

getBytes n =
    unsafePerformIO $
        withFile "/dev/urandom" ReadMode $
            (\h -> BC.hGet h n)

getString filt n =
    filter (\c -> and (map (\f -> f c) filt)) $ BC.unpack $ getBytes n

getRaw = getString [\t -> True]
getAscii = getString [isAscii]
getAlpha = getString [isAscii, isAlpha]
getLower = getString [isAscii, isLower]
getUpper = getString [isAscii, isUpper]
getDigits = getString [isAscii, isDigit]

getBits n = concatMap (\b -> char2bits (ord b)) (getRaw n)

listOfWord64 = do
    empty <- Bin.isEmpty
    if empty
        then return []
        else do
            v <- Bin.getWord64be
            rest <- listOfWord64
            return (v : rest)

listOfWord gw = do
    empty <- Bin.isEmpty
    if empty
        then return []
        else do
            v <- gw
            rest <- listOfWord gw
            return (v : rest)

getWords n =
    unsafePerformIO $ do
        v' <- withFile "/dev/urandom" ReadMode $
            (\h -> do
                v <- BL.hGet h (n*8)
                return v
            )
        return (Bin.runGet listOfWord64 v')

get8 = getX 8
get16 = getX 16
get32 = getX 32
get64 = getX 63

getMix n = concat $ [get8 n, get16 n, get32 n, get64 n]

getX lim n = map (`mod` (lim :: Word64)) $ getWords n

getPair lim =
    let
        (x:y:[]) = getX lim 2
    in
        (x,y)

{- getPairne = get a pair of unequal integers -}
getPairNe lim =
    let
        pair@(x,y) = getPair lim
    in
        if (x == y)
            then getPairNe lim
            else pair


w64ToInt w = fromIntegral w :: Int
intToW64 i = fromIntegral i :: GHC.Word.Word64
