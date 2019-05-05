-- addStuffFunction.hs

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

-- Currying and Uncurrying

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested = \i -> \b -> i +(nonsense b)

-- TypeInference1.hs

f :: Num a => a -> a -> a 
f x y = x + y + 3

-- declaration of triple's type
triple :: Int -> Int
-- declaration of the function
triple x = x * 3

-- Type declaration using where and let
tripleWhere x = tripleItYo x 
    where tripleItYo :: Integer -> Integer
          tripleItYo y = y * 3

-- Exercies: Write a type signature
-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y

-- Exercises: Given a type, write the function
-- 1
i :: a -> a
i x = x

--2
c :: a -> b -> a
c x y = x 

-- 3
c'' :: b -> a -> b
c'' x y = x

-- 4
c' :: a -> b -> b
c' x y = y

-- 5
r :: [a] -> [a]
r x = x ++ x

-- 6
co :: (b -> c) -> (a -> b) -> (a -> c)
co g f x = g (f x)

