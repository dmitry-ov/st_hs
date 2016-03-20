--Определите представителя класса Functor для следующего типа данных,
--представляющего точку в трёхмерном пространстве:




data Point3D a = Point3D a a a deriving Show

--GHCi> fmap (+ 1) (Point3D 5 6 7)
--Point3D 6 7 8

instance Functor Point3D where
   fmap f (Point3D a b c)= (Point3D (f a) (f b) (f c))



--Определите представителя класса Functor для типа данных GeomPrimitive, который определён следующим образом:
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
--При определении, воспользуйтесь тем, что Point3D уже является представителем класса Functor.

--GHCi> fmap (+ 1) $ Point (Point3D 0 0 0)
--Point (Point3D 1 1 1)
--
--GHCi> fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
--LineSegment (Point3D 1 1 1) (Point3D 2 2 2)


instance Functor GeomPrimitive where
   fmap f (Point (Point3D x y z)) = (Point (Point3D (f x) (f y) (f z)))
   fmap f (LineSegment (Point3D x1 y1 z1) (Point3D x2 y2 z2)) = LineSegment (Point3D (f x1) (f y1) (f z1)) (Point3D (f x2) (f y2) (f z2))



--Определите представителя класса Functor sдля бинарного дерева, в каждом узле которого хранятся элементы типа Maybe:
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

--GHCi> words <$> Leaf Nothing
--Leaf Nothing
--
--GHCi> words <$> Leaf (Just "a b")
--Leaf (Just ["a","b"])

instance Functor Tree where
    fmap f (Leaf a) = Leaf $ fmap f a
    fmap f (Branch l a r) = Branch (fmap f l) (fmap f a) (fmap f r)




--Определите представителя класса Functor для типов данных Entry и Map.
--Тип Map представляет словарь, ключами которого являются пары:

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

--В результате должно обеспечиваться следующее поведение:
--fmap применяет функцию к значениям в словаре, не изменяя при этом ключи.
--
--GHCi> fmap (map toUpper) $ Map []
--Map []
--
--GHCi> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
--Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]
--


instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1, k2) $ f v


instance Functor (Map k1 k2) where
    fmap f (Map xs) =  Map (map (fmap f) xs)





--Законы функторов
fmap (f . g) xs = (fmap f . fmap g) xs









