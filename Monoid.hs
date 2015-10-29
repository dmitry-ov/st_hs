newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend = xor


xor :: Xor -> Xor -> Xor
xor (Xor a) (Xor b) = Xor $ xorb a b

xorb :: Bool -> Bool -> Bool
xorb False False = False
xorb True True = False
xorb _ _ = True