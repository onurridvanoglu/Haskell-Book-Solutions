-- Write a Travesable instance for the given datas

-- 1

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldr f z (Identity a) = f a z
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

-- 2
newtype Constant a b = Constant { getConstant :: a}

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr f z (Constant a) = f a z

instance Traversable (Constant a) where
    traverse f (Constant a) = Constant <$> f a
