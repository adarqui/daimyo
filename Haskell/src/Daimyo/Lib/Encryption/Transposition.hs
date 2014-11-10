{-# LANGUAGE RecordWildCards #-}

{-
 - http://en.wikipedia.org/wiki/Transposition_cipher
 - http://scienceblogs.com/goodmath/2008/08/24/transposition-ciphers/
 -}

module Daimyo.Lib.Encryption.Transposition (
 Encryption,
 new,
 enc,
 dec
) where

import Data.Matrix
import Data.List
import qualified Data.Vector as V
import Data.Maybe

data Encryption = Encryption {
 _cipher :: Cipher,
 _key :: String,
 _enc :: EncryptFn,
 _dec :: DecryptFn
}

data Cipher = Rail | Columnar | Route | DoubleC | Myszkowski deriving (Show, Read, Eq, Enum)

type EncryptFn = Encryption -> Plain -> Encrypted
type DecryptFn = Encryption -> Encrypted -> Plain

data Encrypted = Encrypted String
data Plain = Plain String

new :: Cipher -> String -> Encryption
new cipher key =
 Encryption {
  _cipher = cipher,
  _key = key,
  _enc = en,
  _dec = de
 }
 where
  (en,de) = new' cipher

new' :: Cipher -> (EncryptFn, DecryptFn)
new' Columnar = (enc'Columnar,dec'Columnar)
new' _ = error "Unsupported"

enc :: Encryption -> Plain -> Encrypted
enc Encryption{..} text = Encrypted "hi"

dec :: Encryption -> Encrypted -> Plain
dec Encryption{..} text = Plain "hi"

enc'Columnar :: Encryption -> Plain -> Encrypted
enc'Columnar Encryption{..} text = Encrypted "hi"

dec'Columnar :: Encryption -> Encrypted -> Plain
dec'Columnar Encryption{..} text = Plain "hi"

translate :: [Char] -> Matrix (Maybe Char) -> [Char]
translate key mat = catMaybes $ concat $ map (\(c,i) -> V.toList $ getCol i mat) $ tag key

translate' :: [Char] -> Matrix (Maybe Char) -> [[Char]]
translate' key mat = map (\t -> catMaybes $ concat $ map (\(c,i) -> V.toList $ getCol i mat) t) $ permutations $ nub $ sort $ tag' key

columnar :: [Char] -> [Char] -> Matrix (Maybe Char)
columnar key chars = fromList rows columns $ just chars ++ repeat Nothing
 where
  (rows, columns) = mLen key chars

mLen key chars = (div' + fac', length key)
 where
  div' = length chars `div` length key
  rem' = length chars `mod` length key
  fac' = if (rem' > 0) then 1 else 0

just chars = map Just chars

tag key = sortBy (\(c1,i1) (c2,i2) -> compare c1 c2) $ zip key [1..]
tag' key = zip key [1..]
  

{-
 - 123 ABCDEFGHIJ
 - ABC
 - DEF
 - GHI
 - J
 - 5 9
 -}


t1 = translate "ZEBRAS" $ columnar "ZEBRAS" "WEAREDISCOVEREDFLEEATONCE"
