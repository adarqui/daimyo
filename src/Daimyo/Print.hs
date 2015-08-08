module Daimyo.Print (
  showVar,
  showVarP,
  indentSp
) where

import           Data.List

showVar :: (Show a) => String -> a -> String
showVar var a = var ++ "=" ++ show a

showVarP :: (Show a) => String -> a -> String
showVarP var a = '(' : showVar var a ++ ")"

indentSp :: Int -> String -> String
indentSp sp s = replicate sp ' ' ++ s
