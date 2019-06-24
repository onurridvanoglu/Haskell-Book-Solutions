-- MaybeT
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

-- EitherT 
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance (Applicative m) => Applicative (EitherT e m) where
    pure = EitherT . pure . pure
    (EitherT f) <*> (EitherT ema) = EitherT $ (<*>) <$> f <*> ema

instance (Monad m) => Monad (EitherT e m) where
    return = pure 
    (EitherT ema) >>= f = EitherT $ do
        a <- ema
        case a of
            Right x -> runEitherT (f x)
            Left y -> return $ Left y

-- 4
swapEither :: Either a b -> Either b a
swapEither (Left a) = (Right a)
swapEither (Right a) = (Left a)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

-- 5
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fe ga (EitherT ema) = undefined

-- ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure = ReaderT . pure . pure
    (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
    return = pure
    (ReaderT rma) >>= f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r

-- StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT sma) = StateT $ (fmap . fmap) fa sma
        where fa (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)
    (StateT fsma) <*> (StateT sma) = StateT $ \s -> do
        (f, s1) <- fsma s
        (a, s2) <- sma s1
        return $ (f a, s2)

instance (Monad m) => Monad (StateT s m) where
    return = pure
    (StateT sma) >>= f = StateT $ \s -> do
        (a, s1) <- sma s
        runStateT (f a) s1