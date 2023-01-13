import Data.Binary.Get (label)

data BTree = Empty | Node Int BTree BTree

-- helper :: BTree -> Int

helper :: BTree -> Bool
helper (Node num Empty Empty) = True
helper (Node num l@(Node numl _ _) Empty) = num > numl && helper l
helper (Node num Empty r@(Node numr _ _)) = num < numr && helper r
helper (Node num l@(Node numl _ _) r@(Node numr _ _)) = num > numl && helper l && num < numr && helper r

t1 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

t4 = Node 8 (Node 3 Empty Empty) (Node 10 Empty Empty)

t2 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 5 Empty Empty) (Node 14 Empty Empty))

t3 = Node 8 (Node 3 (Node 5 Empty Empty) (Node 6 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))
