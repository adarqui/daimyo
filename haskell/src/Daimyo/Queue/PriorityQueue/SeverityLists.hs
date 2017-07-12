module Daimyo.Queue.PriorityQueue.SeverityLists (
  PQueue,
  empty,
  isEmpty,
  enqueueHigh,
  enqueueMedium,
  enqueueLow,
  dequeue,
  front
) where

-- | PQueue
--
newtype PQueue a
  = PQueue ([a],[a],[a])
  deriving (Show)

-- | empty
--
empty :: PQueue a
empty = PQueue ([],[],[])

-- | isEmpty
--
isEmpty :: PQueue a -> Bool
isEmpty (PQueue ([],[],[])) = True
isEmpty _                   = False

-- | enqueueHigh, enqueueMedium, enqueueLow
--
enqueueHigh, enqueueMedium, enqueueLow :: Ord a => a -> PQueue a -> PQueue a
enqueueHigh   x (PQueue (xs,ys,zs)) = PQueue ((x:xs),ys,zs)
enqueueMedium y (PQueue (xs,ys,zs)) = PQueue (xs,(y:ys),zs)
enqueueLow    z (PQueue (xs,ys,zs)) = PQueue (xs,ys,(z:zs))

-- | dequeue
--
dequeue :: PQueue a -> Maybe (PQueue a)
dequeue (PQueue ([],[],[]))     = Nothing
dequeue (PQueue ((_:xs),ys,zs)) = Just $ PQueue (xs,ys,zs)
dequeue (PQueue ([],(_:ys),zs)) = Just $ PQueue ([],ys,zs)
dequeue (PQueue ([],[],(_:zs))) = Just $ PQueue ([],[],zs)

-- | front
--
front :: PQueue a -> Maybe a
front (PQueue ([],[],[]))    = Nothing
front (PQueue ((x:_),_,_))   = Just x -- High
front (PQueue ([],(y:_),_))  = Just y -- Medium
front (PQueue ([],[],(z:_))) = Just z -- Low
