{-# LANGUAGE RankNTypes#-}

module Daimyo.Lib.Permutation.Heaps (
    heaps,
    heaps'ST
) where

import GHC.Arr
import GHC.ST
import Control.Monad
import Control.Monad.ST
import Data.List



heaps l = heaps'ST l

heaps'ST l = do
    runSTArray $ do
        st <- unsafeThawSTArray $ listArray (0,sz) l
        heaps'ST'loop l sz st
    where
        sz = (length l) - 1

heaps'ST'loop l 0 st = do
    array' <- freezeSTArray st
    let v = [elems array']
    thawSTArray array'
    return v
{-
    array' <- unsafeFreezeSTArray st
    let v = [elems array']
    unsafeThawSTArray array'
    return v
-}

heaps'ST'loop l n st = do
    m <- mapM
            (\i -> do
                k <- heaps'ST'loop l (n-1) st
                let j = if (odd n) then 0 else i
                swap st j n
                return k
            )
        [0..n]
    return $ concat m

swap st i j = do
 i' <- unsafeReadSTArray st i
 j' <- unsafeReadSTArray st j
 unsafeWriteSTArray st i j'
 unsafeWriteSTArray st j i'

runSTArray :: (forall s . ST s [[e]]) -> [[e]]
runSTArray st = runST st

{-
heaps l = heaps'ST l

heaps'ST l = do
    runSTArray $ do
        st <- unsafeThawSTArray $ listArray (0,sz) l
        heaps'ST'loop l sz st
        return st
    where
        sz = (length l) - 1

heaps'ST'loop l n st = return []
heaps'ST'loop (x:xs) n st = return []
-}
   

{-
heaps l = heaps'ST l

heaps'ST l = do
    runSTArray $ do
        st <- thawSTArray $ listArray (0,sz) l
--        st <- unsafeThawSTArray $ listArray (0,sz) l
        heaps'ST'loop l sz st
    where
        sz = (length l) - 1
-}

{-
heaps'ST'loop l 0 st = do
    st' <- unsafeFreezeSTArray st
    return [elems st']

heaps'ST'loop l n st = do
    m <- foldM
            (\(st'acc, l'acc) i -> do
                k <- heaps'ST'loop l (n-1) st'acc
                let j = if (odd n) then 0 else i
                swap st'acc j (n - 1)
                return (st'acc, l'acc ++ k)
            )
        (st, [])
        [0..n]
    return $ snd m
-}
