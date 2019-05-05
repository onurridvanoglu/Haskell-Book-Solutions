module Exercises where

import Data.Char
    
-- 2
upperLetters :: String -> String
upperLetters xs = [x | x <- xs, isUpper x == True]

-- Another way to do this example 
myupperCase :: String -> String
myupperCase [] = []
myupperCase (x:xs)
            | isUpper x = x : myupperCase xs
            | otherwise = myupperCase xs

-- 3
upFirst :: String -> String
upFirst [] = []
upFirst (x:xs) = toUpper x : xs

-- 4
upAll :: String -> String
upAll [] = []
upAll (x:xs) = toUpper x : upAll xs

-- 5
upFirsttake :: String -> Char
upFirsttake (x:xs) = head (toUpper x:xs)

-- Writing your own standart functions
--1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs
    
-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False 
myAny f (x:xs) 
    |f x = True
    |otherwise = False

-- 4 myReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem n (x:xs) 
        | x == n = True
        | otherwise = myElem n xs 

-- 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs





