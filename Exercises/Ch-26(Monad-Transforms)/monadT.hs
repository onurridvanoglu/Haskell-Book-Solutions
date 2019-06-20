-- Maybe Transform
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    -- Below line is the same as: pure x = 
    pure = MaybeT . pure . pure
    (MaybeT f) <*> (MaybeT ma) = MaybeT $ (<*>) <$> f <*> ma
    -- Above line is same as: = MaybeT $ (fmap (<*>) f) <*> ma

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (MaybeT ma) >>= f = 
        MaybeT $ do
            v <- ma
            case v of
                Nothing -> return Nothing
                Just y -> runMaybeT (f y)