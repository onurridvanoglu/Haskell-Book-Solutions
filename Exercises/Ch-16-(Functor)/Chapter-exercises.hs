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

-- Rearrange the arguments
-- 1
data Sum a b = First a | Second b
instance Functor (Sum e) where 
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

-- 2
data Company a b c = DeepBlue a c | Something b
instance Functor (Company e e') where 
    fmap _ (Something b) = Something b
    fmap f (DeepBlue a c) = DeepBlue a (f c)

-- 3
data More a b = L a b a | R b a b deriving (Eq, Show)
instance Functor (More a) where 
    fmap f (L a b a') = L a (f b) a' 
    fmap f (R b a b') = R (f b) a (f b')

-- Write Functor instances
-- 1
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)


