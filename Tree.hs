--Тип бинарных деревьев можно описать следующим образом:

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

--Реализуйте функцию height, возвращающую высоту дерева, и функцию size,
--возвращающую количество узлов в дереве (и внутренних, и листьев).
--Считается, что дерево, состоящее из одного листа, имеет высоту 0.

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + (max (height a) (height b))


size :: Tree a -> Int
size (Leaf a) = 1
size (Node (Leaf a) (Leaf b)) = 3
size (Node a b) = 1 + size a + size b



avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go = undefined


--let t = Node  (Node (Leaf 1) (Node (Leaf  3) (Leaf 7)))  (Leaf 4)

