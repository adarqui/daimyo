module Daimyo.Boxes (
  module A,
  table,
  printSimpleTable,
  printTable,
) where

-- sources:
-- http://www.tedreed.info/programming/2012/06/02/how-to-use-textprettyprintboxes/
-- https://gist.github.com/treed/e5d63c9eff65e59583a6

import           Data.List
import           Text.PrettyPrint.Boxes as A

-- | table
--
table :: Int -> [[String]] -> Box
table n rows = hsep n left (map (vcat left . map text) (transpose rows))

-- | printSimpleTable
--
printSimpleTable :: Int -> [[String]] -> IO ()
printSimpleTable n rows = printBox $ table n rows

-- | printTable
printTable :: [Alignment] -> [String] -> [[String]] -> IO ()
printTable aligns titles rows = printBox $ hsep 2 left (map (\(align, title, col) -> vcat center2 [text title, vcat align $ map text col]) $ zip3 aligns titles (transpose rows))
