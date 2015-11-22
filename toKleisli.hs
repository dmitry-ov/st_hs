toKleisli :: Monad m => (a -> b) -> a -> m b
toKleisli f x = return (f x)