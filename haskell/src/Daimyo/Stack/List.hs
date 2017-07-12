module Daimyo.Stack.List (
  Stack,
  push,
  pop,
  pop',
  top,
  top',
  empty,
  isEmpty,
  size
) where

import           Data.Maybe

-- | Stack
--
newtype Stack a
  = Stack [a]
  deriving (Show)

-- | push
--
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

-- | pop
--
pop :: Stack a -> Maybe (Stack a)
pop (Stack [])     = Nothing
pop (Stack (_:xs)) = Just $ Stack xs

-- | pop'
--
-- dangerous: assumes Stack is not empty
--
pop' :: Stack a -> Stack a
pop' = fromJust . pop

-- | top
--
top :: Stack a -> Maybe a
top (Stack [])    = Nothing
top (Stack (x:_)) = Just x

-- | top'
--
-- dangerous: assumes Stack is not empty
--
top' :: Stack a -> a
top' = fromJust . top

-- | empty
--
empty :: Stack a
empty = Stack []

-- | isEmpty
--
isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _          = False

-- | size
--
size :: Stack a -> Int
size (Stack [])     = 0
size (Stack (_:xs)) = 1 + (size (Stack xs))
