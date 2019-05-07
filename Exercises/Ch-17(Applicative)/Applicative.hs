-- lookup exercises
import Data.List (elemIndex) 
import Control.Applicative

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")] 
h z = lookup z[(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- Short Exercises
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2
y :: Maybe Integer 
y = lookup 3 $ zip [1 , 2, 3] [4, 5, 6]

z :: Maybe Integer 
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer) 
tupled = (,) <$> y <*> z

-- 3
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5] 

y1 :: Maybe Int
y1 = elemIndex 4 [1, 2, 3, 4, 5] 

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int 
maxed = max' <$> x <*> y1 

-- 4
xs = [1, 2, 3]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys 

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys 

summed :: Maybe Integer
summed = sum <$> ((,) <$> x' <*> y)

-- Writing out Applicative instance for Identity 
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where 
    fmap f (Identity a) = Identity (f a)
instance Applicative Identity where 
    pure = Identity
    (Identity f) <*> (Identity b) =  Identity (f b)

-- Writing out Applicative instance for Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)
instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a
instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant x) <*> (Constant y) = Constant $ mappend x y

-- Maybe Applicative
validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Adress = Adress String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAdress :: String -> Maybe Adress
mkAdress a = fmap Adress $ validateLength 100 a

data Person = Person Name Adress deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = 
    case mkName n of
        Nothing -> Nothing
        Just n' -> 
            case mkAdress a of 
                Nothing -> Nothing
                Just a' -> 
                    Just $ Person n' a' 

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAdress a 

-- Data Cow
data Cow = Cow {
     name :: String, 
     age :: Int,
     weight :: Int }
     deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow 
cowFromString name' age' weight' =
    case noEmpty name' of 
        Nothing -> Nothing 
        Just nammy ->
            case noNegative age' of 
                Nothing -> Nothing 
                Just agey ->
                    case noNegative weight' of 
                        Nothing -> Nothing
                        Just weighty ->
                            Just (Cow nammy agey weighty)

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' n a w = 
    Cow <$> noEmpty n 
        <*> noNegative a
        <*> noNegative w 

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' n a w =
    liftA3 Cow (noEmpty n) (noNegative a) (noNegative w)

