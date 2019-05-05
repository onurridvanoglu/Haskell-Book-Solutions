import Data.Monoid
data Optional a = Nada | Only a deriving (Eq, Show)

instance (Semigroup a, Monoid a) => Semigroup (Optional a) where
    (<>) (Only x) (Only y)   = Only $ (<>) x y
    (<>) (Only x)   _        = Only x
    (<>) _        (Only y)   = Only y
    (<>) _        _          = Nada

instance (Semigroup a, Monoid a) => Monoid (Optional a) where
    mempty  = Nada 
    
newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)
instance Semigroup (First' a) where
    (<>) (First' a) (First' b) = First' a 
    (<>) (First' a) _ = First' a 
    (<>) _ (First' a) = First' a
    (<>) _ _ = First' Nada

instance Monoid (First' a) where
    mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend
type FirstMappend = First' String -> First' String -> First' String -> Bool
