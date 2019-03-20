module Queue (
  Queue (Queue)
  , queueSize
  , queuePush
  , queuePop
  , queueEmpty
) where

-- | Data type of queue from two stack
data Queue a
  = Queue [a] [a] Int -- constructor of Queue which realized with two arrays
  deriving Show

-- | Function that return size of queue
queueSize :: Queue a -> Int
queueSize (Queue _ _ x) = x

-- | Function that add element to queue
queuePush :: a -> Queue a -> Queue a
queuePush v (Queue first second size) = Queue (v : first) second (size + 1)

-- | Function that take one element from queue
-- return Nothing if queue is empty
-- otherwise return Just pair of new Queue without this element
-- and decrease queue size and latest element
queuePop :: Queue a -> Maybe (Queue a, a)
queuePop (Queue [] [] _) = Nothing
queuePop (Queue x (y:ys) v) = Just (Queue x ys (v - 1), y)
queuePop (Queue x [] v) =
  let right = reverse x in
    let rightHead = head right in
      let rightTail = tail right in
        Just (Queue [] rightTail (v - 1), rightHead)

-- | Function that create empty queue
queueEmpty :: Queue a
queueEmpty = Queue [] [] 0