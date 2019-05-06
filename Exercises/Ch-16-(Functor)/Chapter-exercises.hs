-- Chapter Exercises 
-- 1

data Bool = False | True 
-- invalid Functor 
-- 2

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
    fmap f (True' a)  = True' (f a)
    fmap f (False' a) = False' (f a)

-- 3
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
    fmap f (Truish a) = Truish (f a)
    fmap _ Falsish = Falsish

