-- intermission: Exercise
class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

-- 1
data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

-- 2
data Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

-- 3
data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 5 
data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

-- 6
data Quadriceps a b c d = Quadriceps a b c d deriving (Eq, Show)




