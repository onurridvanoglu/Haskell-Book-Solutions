-- Monad instances
-- 1

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope  where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= _ = NopeDotJpg
    
-- 2

data PhbtEither b a = Lefty a | Righty b

instance (Semigroup a, Semigroup b) => Semigroup (PhbtEither b a) where
    (Righty a) <> (Righty b)= Righty (a <> b)
    (Lefty a) <> (Lefty b) = Lefty (a <> b)
    (Righty a) <> _ = Righty a
    _ <> (Righty a) = Righty a
    
instance (Monoid a, Monoid b) => Monoid (PhbtEither b a) where
    mappend = (<>)
    mempty = Lefty mempty

instance Functor (PhbtEither a) where
    fmap f (Lefty a) = Lefty (f a)
    fmap _ (Righty b) = Righty b

instance Monoid a => Applicative (PhbtEither a) where
    pure a = Lefty a
    (Lefty f) <*> (Lefty b) = Lefty (f b)
    (Righty a) <*> (Righty b) = Righty (a <> b)
    (Righty a) <*> _ = Righty a
    _ <*> (Righty a) = Righty a


instance Monoid a => Monad (PhbtEither a) where
    return = pure
    (Righty a) >>= _ = Righty a
    (Lefty a) >>= f = f a

-- 3

data Identity a = Identity a deriving (Eq, Show, Ord)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity 
    (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure 
    (Identity a) >>= f = (f a)
    
-- 4
data List a = Nil | Cons a (List a)

instance Semigroup a => Semigroup (List a) where
    (Cons a a') <> (Cons b) = Cons a (a' <> b)
    _ <> Nil = Nil
    Nil <> _ = Nil

instance Monoid a => Monoid (List a) where
    mempty = Nil
    mappend = (<>)

