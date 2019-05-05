fibs = 1 : scanl (+) 1 fibs
fibsN :: Int -> Int
fibsN x = fibs !! x
f = take 20 $ fibs

facto = scanl (*) 1 [2..]

stops = "pbtdkg"
vowels = "aeiou"

writeTuple3 = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x xs -> (f x) || xs) False

myElem :: Eq a => a -> [a] -> Bool
myElem n = foldr (\x xs -> x == n || xs) False 

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

myReverse' :: [a] -> [a]
myReverse' = foldl (\xs x -> x : xs) [] -- Faster

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> (f x) : xs) []

-- Another solution to myMap 
myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x : xs else xs) []

squish :: [[a]] -> [a] 
squish = foldr (\x xs -> x ++ xs) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x xs -> (f x) ++ xs) []

-- Another application of this example 
squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = squish . myMap f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = foldl fn x xs
    where fn a b = case f a b of
                    GT -> a
                    _ -> b

-- another application of myMax
myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f xs = foldr (\a b -> if f a b == GT then a else b) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs

