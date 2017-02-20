module Q(QUEUE,Empty,Enter,qremove,isempty,qmember) where

data QUEUE a = Queue [a] [a]
Empty = Queue [] []
Enter (Queue outl inl) x=Queue outl (x:inl)
isempty (Queue [] [])=True
isempty _ = False
qremove (Queue (a:outl) inl) = (a,Queue outl inl)
qremove (Queue [] []) = error "remove from empty queue"
qremove (Queue [] inl) = qremove (Queue (reverse inl) [])
qmember (Queue outl inl) x =elem x outl || elem x inl

{-end-}
