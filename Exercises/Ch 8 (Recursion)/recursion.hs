-- Recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: Int -> Int -> Int
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1 
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
--isPalindrome x = (head x == last x) && (isPalindrome(init (tail x)))
isPalindrome x = (head x == last x) && (isPalindrome $ init $ tail x)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--Exercises
--McCarthy91
mc91 :: Int -> Int
mc91 99 = 91
mc91 n 
    | n > 100 = n - 10
    | otherwise = mc91(mc91(n + 11))

-- Recursion exercises
mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
--mySum 1 = 1
mySum n = n + mySum (n - 1)

myMult :: (Integral a) => a -> a -> a   
myMult 0 n = 0
myMult 1 n = n
myMult m 0 = 0
myMult m 1 = m
myMult m n = m + myMult m (n - 1)
