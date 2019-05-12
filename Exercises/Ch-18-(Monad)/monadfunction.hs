-- Monad function exercises

j :: Monad m => m (m a) -> m a
j x = x >>= id
-- j x = do
--  x' <- x
--  x

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
-- l1 f x = do
--    a <- x
--    return $ f a


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = do
    x <- a
    y <- b
    return $ f x y

a :: Monad m => m a -> m (a -> b) -> m b
a a f = do
    x <- a
    g <- f
    return $ g x
    
a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip (<*>)     -- Since (<*>) :: Applicative f => f (a -> b) -> f a -> f b


