module Daimyo.Proof.KnowShow (
    KnowShow (..),
    knowShow,
    t_knowShow,
    printit
) where

import Daimyo.Boxes
import Data.Matrix

data KnowShow = KnowShow {
    title :: String,
    p :: [[String]],
    q :: [[String]],
    m :: Matrix String
} deriving (Show)

knowShow title ps qs =
    let
        firstRow = [ "Step", "Know", "Reason" ]
        ps' = map (\((x,y),n) -> ["P"++show n, x, y]) $ zip ps [0..]
        qs' = map (\((x,y),n) -> ["Q"++show n, x, y]) $ zip qs [0..]
    in
        KnowShow {
            title = title,
            p = ps',
            q = qs',
            m = fromLists $ [firstRow] ++ ps' ++ qs'
        }

printit ks = printSimpleTable 5 $ toLists $ m ks


t_knowShow =
    printit $
    knowShow
        "if x and y are odd integers, then x * y is an odd integer"
        [
            ("x and y are odd integers.", "Hypothesis"),
            ("There exists integers m and n such that x = 2m + 1 and y = 2n + 1", "Definition of an odd integer"),
            ("xy = (2m + 1)(2n + 1)", "Substitution"),
            ("xy = 4mn + 2m + 2n + 1", "Algebra"),
            ("xy = 2(2mn + m + n) + 1", "Algebra"),
            ("(2mn + m + n) is an integer.", "Closure properties of the integers")
        ]
        [
            ("There exists an integer q such that xy = 2q + 1", "Use q = (2mn + m + n)"),
            ("x * y is an odd integer.", "Definition of an odd integer")
        ]
