module Daimyo.Lib.Wave.GroupTheory.Tables (
    Group (..),
    additive'Group,
    multiplicative'Group,
    p,
    mat,
    char,
    explain,
    explain'mul,
    explain'add,
    explain'div,
    explain'sub,
    mul,
    add,
    sub,
    div,
    is'Group,
    toList
) where

import Data.Matrix
import Data.List
import Prelude hiding (div)
import qualified Prelude as P


data Group a = Group {
    op :: a -> a -> a,
    e :: a,
    inv :: a -> a,
    table :: Matrix a,
    invTable :: Matrix a,
    range :: [a]
}

additive'Group z = new'Group (+) 0 (\a -> (-a)) [1..z]

multiplicative'Group z = new'Group (*) 1 (\a -> (1/a)) [2..z]

new'Group :: (a -> a -> a) -> a -> (a -> a) -> [a] -> Group a
new'Group op e inv range = Group {
    op = op,
    e = e,
    inv = inv,
    table = mat' op e range,
    invTable = mat'inv op inv range,
    range = range
}

is'Group g =
    let
        t = table g
        it = invTable g
        e' = e g
        invlist = toList it
    in
        (getRow 1 t == getCol 1 t) && (length (nub invlist) == 1)
 
mat' op e range =
    let
        values = [ [ x `op` y | x <- e : range ] |  y <- e : range ]
    in
        fromLists values

mat'inv op inv range =
    let
        values = [ [ let r = (x `op` y) in r `op` (inv r) | x <- range ] |  y <- range ]
    in
        fromLists values

p :: Show a => Matrix a -> String
p = prettyMatrix

mul :: Int -> Int -> Matrix Int
mul = mat (*) 1

add :: Int -> Int -> Matrix Int
add = mat (+) 0

div :: Int -> Int -> Matrix Int
div = mat P.div 1

sub :: Int -> Int -> Matrix Int
sub = mat (-) 0

mat :: (Int -> Int -> Int) -> Int -> Int -> Int -> Matrix Int
mat op identity i j =
    let
        values = [ [ x `op` y | x <- [identity..i] ] |  y <- [identity..j] ]
    in
        fromLists values

explain'mul = explain '*'
explain'add = explain '+'
explain'div = explain '/'
explain'sub = explain '-'

char :: Char -> Char -> Matrix String
char i j =
    let
        values = [ [ x:y:[] | x <- '.' : ['a'..i] ] |  y <- '.' : ['a'..j] ]
    in
        fromLists values

explain :: Char -> Char -> Char -> Matrix String
explain op i j =
    let
        values = [ [ x : op : y : [] | x <- '.' : ['a'..i] ] |  y <- '.' : ['a'..j] ]
    in
        fromLists values
