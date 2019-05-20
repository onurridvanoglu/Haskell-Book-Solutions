-- Foldable exercises

-- 1
sum' :: (Foldable t, Num a) => t a -> a
sum' x = foldr (+) 0 x

-- 2
product' :: (Foldable t, Num a) => t a -> a
product' x = foldr (*) 1 x

-- 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a b = foldr (\x y -> x == a || y) False b

-- 6
null' :: (Foldable t) => t a -> Bool
null' x = foldr (\_ _ -> False) True x

-- 7
length' :: (Foldable t) => t a -> Int
length' x = foldr (\_ n -> n + 1) 0 x

-- 8 
toList' :: (Foldable t) => t a -> [a]
toList' a = foldr (\x xs -> x : xs) [] a

-- 9 
fold' :: (Foldable t, Monoid m) => t m -> m
fold' a = foldr (\x y -> x <> y) mempty a

