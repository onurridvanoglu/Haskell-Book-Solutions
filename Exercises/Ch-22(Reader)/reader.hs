
import Control.Applicative
import Data.Char

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

-- Here fmap composes two functions before applyİng 
-- them to the argument
m' :: Integer -> Integer
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

hurrDurr :: Integer -> Integer 
hurrDurr = do
    a <- hurr
    b <- durr
    return (a + b)

-- Short Exercise

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap 

newtype Reader r a = Reader { runReader :: r -> a}

-- Exercise 

ask :: Reader a a
ask = Reader id

-- Function applicative 
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
-- To tidy up, we'll make two record types

data Person = 
    Person {
          humanName :: HumanName
        , dogName :: DogName
        , address :: Address
    } deriving (Eq, Show)

data Dog = 
    Dog {
          dogsName :: DogName
        , dogsAddress :: Address
    } deriving (Eq, Show)