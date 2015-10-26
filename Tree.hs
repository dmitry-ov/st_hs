data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + (max (height a) (height b))


size :: Tree a -> Int
size (Leaf a) = 0
size (Node (Leaf a) (Leaf b)) = 1
size (Node a b) = 1 + size a + size b

-- let t = Node  (Node (Leaf 1) (Node (Leaf  3) (Leaf 7)))  (Leaf 4)

