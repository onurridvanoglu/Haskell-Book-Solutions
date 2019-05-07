-- Exercises
-- 1 (for type [])

listPure :: a -> [a]
listPure = pure

listApp :: [(a -> b)] -> [a] -> [b]
listApp = (<*>)

-- 2 (for type IO)
iOPure :: a -> IO a
iOPure = pure

iOApp :: IO (a -> b) -> IO a -> IO b
iOApp = (<*>)

-- 3 (for type (,) a)
tuplePure :: Monoid a => b -> (a, b)
tuplePure = pure

tupleApp :: Monoid a => (a, (b -> c)) -> (a, b) -> (a, c)
tupleApp = (<*>)

-- 4 (for type -> e)
funcPure :: a -> (e -> a)
funcPure = pure

funcApp :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
funcApp = (<*>)