--defining arguments
myNum :: Integer
myNum = 1

myVal f = myNum

--Binding Variables to Values
bindExp :: Integer -> String
bindExp x = let y = 5 in
                "the integer was: " ++ show x
                ++ " and y was: " ++ show y

-- 
bindExp1 :: Integer -> String
bindExp1 x = let z = y + x ; y = 5 in
             let y = 5 in "the integer was: "
             ++ show x ++ " and y was: "
             ++ show y ++ " and z was: " ++ show z

-- Shadowing case
bindExp2 :: Integer -> String
bindExp2 x = let x = 10; y = 5 in
             "the integer was: " ++ show x
             ++ " and y was: " ++ show y

-- Intermission: Exercise page 262
-- 2(a)
{- addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where (\n -> n + 1) --where f n = n + 1 -}

-- 2(b)
--addFive x y = (if x > y then y else x) + 5
--addFive = \x -> \y -> (if x > y then y else x) + 

-- 2(c)
--mflip f = \x -> \y -> f y x
--mflip f x y = f y x

-- Pattern Matching
isItTwo :: Integer -> Bool
isItTwo 2 = True 
isItTwo _ = False

--Case expression
isPalindrome :: Eq a => [a] -> [Char]
isPalindrome x = case x == reverse x of
                True  -> "Yes this word is palindrome"
                False -> "No this word is not a fucking palindrome"

amkBerkay :: String -> String
amkBerkay x = case y of
            True  -> "Berkay'in ben AQ" 
            False -> "Berkay tam bir gay dostum!"
        where y = x == "berkay"

-- Intermission: Exercises
functionC :: (Ord a) => a -> a -> a 
functionC x y = case x > y of
                True  -> x
                False -> y

-- 2
ifEvenAdd2 :: Int -> Int
ifEvenAdd2 x = case even x of 
            True  -> x + 2
            False -> x

-- 3 
nums :: (Ord a, Num a) => a -> a
nums x =case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

--HOFs
data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)
reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
    putStrLn $ show e ++ " is the boss of " ++ show e'
employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
                    GT -> reportBoss e e'
                    EQ -> putStrLn "Neither employee is the boss"
                    LT -> (flip reportBoss) e e'

-- Exercise
dodgy :: Num a => a -> a -> a 
dodgy x y = x+ y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- Guards
myAbs :: Integer -> Integer
myAbs x
    | x < 0 = (-x)
    | otherwise = x

bloodNa :: Integer -> String
bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"

dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
    | x <= 0 = 0
    | x <= 1 = x * 15
    | x <= 2 = x * 12
    | x <= 4 = x * 8
    | otherwise = x * 6

-- Intermission: exercises
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    | otherwise = 'F'
    where y = x / 100

pal :: (Eq a) => [a] -> [Char]
pal x
    | x == reverse x = "Palindrome"
    | otherwise      = "Not a palindrome"

numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1


-- Chatper Exercises, Let's write a code
-- 1 
tensDigit :: Integral a => a -> (a, a) 
{- tensDigit x = d 
    where xLast = div x 10
          d     =  mod xLast 10 -}
tensDigit x = divMod x 10

-- 2
foldBool :: a -> a -> Bool -> a 
foldBool x y z = case z of
                True  -> x
                False -> y
            
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z
        | z == True = x
        | otherwise = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4 

    -- id :: a -> a
    -- id x = x
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show 
main = do
    print (roundTrip 4 :: Int)
    print (id 4)
