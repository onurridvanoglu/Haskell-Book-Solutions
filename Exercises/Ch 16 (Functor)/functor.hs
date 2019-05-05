data FixMePls a = FixMe | Pls a 
    deriving (Eq, Show)

instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)


data WhoCares a = ItDoesnt | Matter a | WhatIsCalled 
    deriving (Eq, Show)

instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt 
    fmap _ WhatIsCalled = WhatIsCalled
    fmap f (Matter a) = Matter (f a)

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)


