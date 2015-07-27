{-# LANGUAGE RankNTypes #-}

module Daimyo.Permutation.Heaps (
  heaps,
  heapsST,
  heapsSTLoop,
  swap
) where

import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           GHC.Arr
import           GHC.ST

-- | heaps
--
-- >>> heaps [1,2,3] :: [[Int]]
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
--
heaps :: [a] -> [[a]]
heaps = heapsST

-- | heapsST
--
-- >>> heapsST [1,2,3] :: [[Int]]
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
--
heapsST :: [a] -> [[a]]
heapsST l = do
  runSTArray $ do
    st <- unsafeThawSTArray $ listArray (0,sz) l
    heapsSTLoop l sz st
  where
    sz = (length l) - 1

-- | heapsSTLoop
--
heapsSTLoop :: Ix i => t -> Int -> STArray s i e -> ST s [[e]]
heapsSTLoop _ 0 st = do
  array' <- freezeSTArray st
  let v = [elems array']
  thawSTArray array'
  return v
heapsSTLoop l n st = do
  m <- mapM
       (\i -> do
         k <- heapsSTLoop l (n-1) st
         let j = if (odd n) then 0 else i
         swap st j n
         return k
       ) [0..n]
  return $ concat m

-- | swap
--
swap :: Ix i => STArray s i e -> Int -> Int -> ST s ()
swap st i j = do
  i' <- unsafeReadSTArray st i
  j' <- unsafeReadSTArray st j
  unsafeWriteSTArray st i j'
  unsafeWriteSTArray st j i'

-- | runSTArray
--
runSTArray :: (forall s . ST s [[e]]) -> [[e]]
runSTArray = runST
