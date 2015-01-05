module Daimyo.Matrix (
    size,
    det'2x2,
    is'2x2,
    abcd'2x2,
    inverse'2x2,
    rcStr,
    label,
    label'to'Map
) where

import Data.Matrix
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.Map as M

newtype LabelMatrix a = LabelMatrix (Matrix (String, a)) deriving (Show)

size m = (nrows m, ncols m)


label :: Char -> Matrix a -> LabelMatrix a
label c m =
    let
        nrows' = nrows m
        ncols' = ncols m
        rows = map (\row -> let row' = getRow row m in map (\(e,i) -> (rcStr c row i, e)) $ zip (V.toList row') [1..ncols']) [1..nrows']
    in
        LabelMatrix $ fromLists rows

label'to'Map c m = let (LabelMatrix m') = label c m in (M.fromList $ toList m')
        

rcStr c row col = c : show row ++ "," ++ show col


det'2x2 m
    | is'2x2 m =
        let
            (a,b,c,d) = abcd'2x2 m
        in
            Just $ 1 / (a*d - b*c)
    | otherwise = Nothing

is'2x2 m
    | size m == (2,2) = True
    | otherwise = False

abcd'2x2 m =
    let
        a = m ! (1,1)
        b = m ! (1,2)
        c = m ! (2,1)
        d = m ! (2,2)
    in
        (a,b,c,d)

inverse'2x2 m
    | is'2x2 m && det'2x2 m /= Nothing =
        let
            (a,b,c,d) = abcd'2x2 m
            m' = fromList 2 2 [d,(-1)*b,(-1)*c,a]
        in
            Just $ scaleMatrix (fromJust (det'2x2 m)) m'
    | otherwise = Nothing


-- random tests

t_det_inverse'1 =
    let m = fromList 2 2 [11, -5, 2, -1] in (det'2x2 m, inverse'2x2 m)

t_det_inverse'2 =
    let m = fromList 2 2 [-3, 3, 8, 7] in (det'2x2 m, inverse'2x2 m)

t_m = fromLists $ [[1,2,3,4],[4,5,6,7],[8,9,10,11]]
t_label'1 = label 'a' t_m
t_label'2 = label'to'Map 'a' t_m
