{-# LANGUAGE InstanceSigs #-}

-- Identity 
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity 
    (Identity f) <*> (Identity a) = Identity (f a)

instance Foldable Identity where
    foldr f acc (Identity a) = f a acc

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

--Compose 
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)
-- We have to fmap twice to get to that value inside because of the layered structures.
-- f and g both have to be the part of the structure that we're lifting over
-- so they both have to be Functors themselves.
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure
    (Compose f) <*> (Compose a) = Compose $ (fmap (<*>) f) <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose a) = (foldMap . foldMap) f a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose a) = Compose <$> (traverse . traverse) f a

-- We can generalize this to different amounts of structure, such as with one less bit of structure. 
newtype One f a = One (f a) deriving (Eq, Show)

instance (Functor f) => Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

-- Or one more layer of structure than Compose:
newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha