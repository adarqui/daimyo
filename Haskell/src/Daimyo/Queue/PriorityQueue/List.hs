module Daimyo.Queue.PriorityQueue.List (
  ExamplePriority (..),
  ExamplePacket (..),
  PQueue,
  empty,
  isEmpty,
  enqueue,
  enqueueBy,
  dequeue,
  front
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
enqueue = enqueueBy (\a b -> a <= b)

-- | enqueueBy
--
enqueueBy :: (a -> a -> Bool) -> a -> PQueue a -> PQueue a
enqueueBy cmp x (PQueue q) = PQueue (insert q)
  where
    insert [] = [x]
    insert qall@(e:es)
      | cmp x e   = x : qall
      | otherwise = e : insert es

-- | dequeue
--
dequeue :: PQueue a -> Maybe (PQueue a)
dequeue (PQueue [])     = Nothing
dequeue (PQueue (_:xs)) = Just $ PQueue xs

-- | front
--
front :: PQueue a -> Maybe a
front (PQueue [])    = Nothing
front (PQueue (x:_)) = Just x
