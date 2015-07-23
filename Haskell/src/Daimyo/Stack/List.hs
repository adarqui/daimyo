module Daimyo.Stack.List (
  Stack,
  push,
  pop,
  top,
  empty,
  isEmpty,
  size
) where

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

-- | top
--
top :: Stack a -> Maybe a
top (Stack [])    = Nothing
top (Stack (x:_)) = Just x

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
