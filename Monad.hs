data SomeType a = SomeType a  deriving Show

instance Functor SomeType where
    fmap f x = x >>= return . f