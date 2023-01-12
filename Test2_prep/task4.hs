import Text.XHtml (height)
data BTree a = Nil | Node a (BTree a) (BTree a)
isPerfectlyBalancedHelper :: BTree a -> Int
isPerfectlyBalancedHelper Nil = 0
isPerfectlyBalancedHelper (Node a left Nil) = isPerfectlyBalancedHelper left
isPerfectlyBalancedHelper (Node a Nil right) = isPerfectlyBalancedHelper right
isPerfectlyBalancedHelper (Node a left right) = (1 + isPerfectlyBalancedHelper left) + (1 + isPerfectlyBalancedHelper right)

heightTree :: BTree a -> Int
heightTree Nil = 0
heightTree (Node a left right) = max (1 + heightTree left) (1 + heightTree right)

isPerfectlyBalanced :: BTree a -> Bool
isPerfectlyBalanced tRee = 2^(heightTree tRee) == (isPerfectlyBalancedHelper tRee + 2)

-- poopyTree = Node 1 (Node 2 (Node 5 Nil Nil) (Node 6 Nil Nil)) (Node 2 (Node 5 Nil Nil) (Node 6 Nil Nil))
tree1 = (Node 17 (Node 14 Nil (Node 7 Nil Nil)) (Node 20 Nil Nil))
