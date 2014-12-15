{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Daimyo.Fun.Alphabet (
 XAlphabet (..)
) where

import Data.Char

newtype XAlphabet = XAlphabet Char deriving (Show, Read, Eq, Ord)

instance Enum XAlphabet where
 succ (XAlphabet c) = XAlphabet 'a'
{-
 0 - 48
 9 - 57
 A - 65
 Z - 90
 a - 97
 z - 122
-}
