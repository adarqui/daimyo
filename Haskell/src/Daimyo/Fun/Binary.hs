{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Daimyo.Fun.Binary (
 XBinary (..)
) where

data Binary = O | Z deriving (Show, Read, Eq, Enum, Ord)

newtype XBinary = XBinary Int deriving (Show, Read, Eq, Num, Ord)

instance Enum XBinary where
 succ 0 = 1
 succ 1 = 0
 succ _ = error "Neither 1 nor 0"
