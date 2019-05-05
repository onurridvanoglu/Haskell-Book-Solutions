--Fractional application
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

--working with Show
data Mood = Blah deriving Show

-- Typeclass instances are dispatched by type
class Numberish a where
    fromNumber :: Integer -> a 
    toNumber :: a -> Integer

--pretend newtype is data for now
newtype Age =
    Age Integer
    deriving (Eq, Show)

instance Numberish Age where    
    fromNumber n = Age n 
    toNumber (Age n) = n 

newtype Year =
    Year Integer
    deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Year n 
    toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a 
sumNumberish a a' = fromNumber summed
    where integerOfA      = toNumber a
          integerOfAPrime = toNumber a'
          summed          = integerOfAPrime + integerOfA

-- Eq instances
data Trivial =
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

-- New example of instances
{-data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun 

data Date = 
    Date DayOfWeek Int 

instance Eq DayOfWeek where
    (==) Mon Mon   = True
    (==) Tue Tue   = True
    (==) Weds Weds = True
    (==) Thu Thu   = True
    (==) Fri Fri   = True
    (==) Sat Sat   = True
    (==) Sun Sun   = True
    (==) _ _       = False-}

-- :set -Wall examples 
f :: Int -> Bool
f 1 = True 
f 2 = True 
f 3 = True 
f _ = False

-- Sometimes we need to ask for more
data Identity a =
    Identity a
instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

-- Intermission exercise
-- 1
data TisAnInteger = 
    TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y

-- 2 
data TwoIntegers = 
    Two Integer Integer 
instance Eq TwoIntegers where
    (==) (Two x y) (Two g f) = (x, y) == (g, f)

-- 3
data StringOrInt =
    TisAnInt    Int
   | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt y)     = x == y
    (==) (TisAString x) (TisAString y) = x == y
    (==) _ _                           = False

-- 4 
data Pair a = 
    Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair f g) = (x, y) == (f, g)

-- 5
data Tuple a b = 
    Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple f g) = (x, y) == (f, g)

-- 6
data Which a =
    ThisOne a
  | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne f) (ThatOne g) = f == g
    (==) _ _                     = False

-- 7
data EitherOr a b =
    Hello a
  | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y)     = x == y
    (==) (Goodbye f) (Goodbye g) = f == g
    (==) _ _                     = False

-- Ord Instances
{- data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Ord, Show, Eq) if we didnt add deriving Eq
    code wouldnt work, but this data only works ordering
    according ot the alphabet  -}

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Eq, Show)
instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ

-- Gimme more operations
add :: Num a => a -> a -> a
add x y = x + y

-- addWeird.hs
addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
    if x > 1
    then x + y
    else x



