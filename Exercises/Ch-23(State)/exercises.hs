-- Chapter exercises

newtype State s a = State { runState :: s -> (a, s)}

instance Functor (State s) where
    fmap f (State g) = State $ \s -> let (a, s1) = g s
                                     in (f a, s1)

instance Applicative (State s) where
    pure a = State $ \s -> let (purea, s1) = (a, s)
                           in (purea, s1)

    (State f) <*> (State g) = State $ \s -> let (a, s1) = g s
                                                (h, s2) = f s1
                                            in (h a, s2)

instance Monad (State s) where
    return = pure 
    (State f) >>= g = State $ \s -> let (a, s1) = f s
                                    in runState (g a) s1

-- 1
get :: State s s
get = State (\s -> (s,s))

-- 2
put :: s -> State s ()
put a = State (\s -> ((), a)) 

-- 3
exec :: State s a -> s -> s
exec (State sa) s = snd (sa s)

-- 4
eval :: State s a -> s -> a
eval (State sa) s = fst (sa s)

-- 5 
modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))