module Daimyo.Boxes (
    table,
    print'table,
    print_table,
    left',
    right',
    center'
) where

{-
    source:
        http://www.tedreed.info/programming/2012/06/02/how-to-use-textprettyprintboxes/
        https://gist.github.com/treed/e5d63c9eff65e59583a6
-}

import Data.List
import Text.PrettyPrint.Boxes

table n rows = hsep n left (map (vcat left . map text) (transpose rows))
print'table n rows = printBox $ table n rows

print_table :: [Alignment] -> [String] -> [[String]] -> IO ()
print_table aligns titles rows = printBox $ hsep 2 left (map (\(align, title, col) -> vcat center2 [text title, vcat align $ map text col]) $ zip3 aligns titles (transpose rows))
 
left' = left
right' = right
center' = center2
