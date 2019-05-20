-- Foldable instances

-- 1
data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
    foldMap f (Constant a) = f a

-- 2 
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
    foldr f z (Two _ b) = f b z
    foldMap f (Two _ b) = f b

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
    foldr f z (Three _ _ c) = f c z
    foldMap f (Three _ _ c) = f c

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
    --foldr f z (Three' _ b c) = (f b z) <> (f c z)
    foldMap f (Three' _ b c) = (f b) <> (f c)

-- 5
data Four a b = Four a b b b

instance Foldable (Four a) where
    foldMap f (Four _ b c d) = (f b) <> (f c) <> (f d)

