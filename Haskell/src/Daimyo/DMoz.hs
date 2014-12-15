module Daimyo.DMoz (
) where

import Prelude hiding (length)

import Data.List

{- Length functions, decompoisition -}

length' [] = 0
length' (_:xs) = 1 + length' xs

len' =
    length "abc" + length "bc" + length "c" + length ""


leng [] = 0
leng (1:[]) = 1 + leng []
leng (1:2:[]) = 1 + leng (1:[])
leng (1:2:3:[]) = 1 + leng (1:2:[])
leng (x:xs) = 1 + leng xs

leng' [] = 0
leng' (x:[]) = 1 + leng' []
leng' (x:y:[]) = 1 + leng' (x:[])
leng' (x:y:z:[]) = 1 + leng' (x:y:[])
leng' (x:xs) = 1 + leng' xs

leng'' [] = 0
leng'' (_:[]) = 1 + leng'' []
leng'' (_:y:[]) = 1 + leng'' (y:[])
leng'' (_:y:z:[]) = 1 + leng'' (y:z:[])
leng'' (_:xs) = 1 + leng'' xs

lengc z = case z of
    [] -> 0
    (x:[]) -> 1
    (x:y:[]) -> 2
    (x:y:z:[]) -> 3
    (x:xs) -> 1 + lengc xs

len [] = 0
len (x:xs) = let rest = xs in 1 + len rest 

listCompEx = "hello"

main :: IO ()
main = do
    let listCompEx = "world" -- local
    putStrLn "hello"

ghci :: IO ()
ghci = do
    let blah = 1
    putStrLn "hi"

{- Local bindings explanation -}

local_bindings_function =
    let x = 1 in
    let y = 2 in
    let z = 3 in
    x + y + z

local_bindings_function' =
    let x = 1
        y = 2
        z = 3
    in
    x + y + z

local_bindings_function'' =
    x + y + z
    where
        x = 1
        y = 2
        z = 3
