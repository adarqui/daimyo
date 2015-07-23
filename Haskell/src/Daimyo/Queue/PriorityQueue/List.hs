module Daimyo.Queue.PriorityQueue.List (
  ExamplePriority (..),
  ExamplePacket (..),
  PQueue,
  empty,
  isEmpty,
  enqueue,
  dequeue
) where

-- | ExamplePriority
--
data ExamplePriority
  = EPSevere
  | EPHigh
  | EPMedium
  | EPLow
  deriving (Eq, Ord, Show, Enum)

-- | ExamplePacket
--
data ExamplePacket a = EPNode ExamplePriority a
  deriving (Eq, Ord, Show)

-- | PQueue
--
newtype PQueue a
  = PQueue [a]
  deriving (Show)

-- | empty
--
empty :: PQueue a
empty = PQueue []

-- | isEmpty
--
isEmpty :: PQueue a -> Bool
isEmpty (PQueue []) = True
isEmpty _           = False

-- | enqueue
--
-- >>> enqueue (EPNode EPSevere "servere1") $ enqueue (EPNode EPHigh "high2") $ enqueue (EPNode EPLow "low1") $ enqueue (EPNode EPMedium "med1") $ enqueue (EPNode EPHigh "high1") empty
-- PQueue [EPNode EPSevere "servere1",EPNode EPHigh "high1",EPNode EPHigh "high2",EPNode EPMedium "med1",EPNode EPLow "low1"]
--
enqueue :: Ord a => a -> PQueue a -> PQueue a
enqueue x (PQueue q) = PQueue (insert x q)
  where
    insert x [] = [x]
    insert x qall@(e:es)
      | x <= e    = x : qall
      | otherwise = e : insert x es

-- | dequeue
--
dequeue :: PQueue a -> Maybe (PQueue a)
dequeue (PQueue [])     = Nothing
dequeue (PQueue (x:xs)) = Just $ PQueue xs

-- | bottom
--
bottom :: PQueue a -> Maybe a
bottom (PQueue [])    = Nothing
bottom (PQueue (x:_)) = Just x
