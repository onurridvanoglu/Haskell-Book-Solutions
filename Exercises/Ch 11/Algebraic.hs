{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char

data Price = 
    Price  Integer deriving (Eq, Show)

data Manufacturer =
                Mini
              | Mazda
              | Tata
                deriving (Eq, Show)

data Airline = 
        PapuAir
        |CatapultsR'Us
        |TakeYourChancesUnited
        deriving (Eq, Show)

data Vehicle = Car Manufacturer Price 
            | Plane Airline
              deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
    
data Example = MakeExample Int deriving Show

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cows  = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool
instance TooMany Int where
  tooMany n = n > 42
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

newtype Group = Group (Int, Int) deriving Show

instance TooMany Group where
  tooMany (Group (x, y)) = x + y > 42


--data Person = MkPerson String Int deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

namae :: Person -> String
namae (Person s _) = s 

data Person =
  Person { name :: String
         , age :: Int }
  deriving (Eq, Show)



-- Language Exercises
-- Using import Data.Char
capitalizeWord :: String -> String 
capitalizeWord (x:xs)
              | mod (ord x) 95 < 27 = toUpper x : xs 
              | otherwise = (x:xs)

capitalizeWord' :: String -> String
capitalizeWord' [] = []
capitalizeWord' (x:xs) = toUpper x : xs

