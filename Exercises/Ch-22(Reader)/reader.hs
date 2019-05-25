{-# LANGUAGE InstanceSigs #-}

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

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ (f . ra)

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

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

onur :: Person
onur = Person (HumanName "Onur Ridvanoglu")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- Exercise 
-- 1
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

-- 2
asks :: (r -> a) -> Reader r a
asks f = Reader (f . id)

-- 3 (Write the Applicative instance for Reader)

-- Monads of functions
foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- We abstract this out so it's not specific to these functions
-- Here r represents the argument that both of our functions are waiting on
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

