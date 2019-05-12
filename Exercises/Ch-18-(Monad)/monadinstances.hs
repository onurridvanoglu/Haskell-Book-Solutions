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


