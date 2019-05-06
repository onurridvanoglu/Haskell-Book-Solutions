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

-- 2
data K a b = K a deriving (Eq, Show)
instance Functor (K a) where
    fmap f (K a) = K a

-- 4 
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a = LiftItOut (f a)
instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut a) = LiftItOut (fmap f a)

-- 6
data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa g a) = DaWrappa (fmap f g) (fmap f a)

-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething g a) = IgnoringSomething g (fmap f a)

-- 8
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g a b) where
    fmap g (Notorious o a t) = Notorious o a (fmap g t)
    
-- 9
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

-- 10
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats l m r) = MoreGoats (fmap f l) (fmap f m) (fmap f r)

-- 11 
