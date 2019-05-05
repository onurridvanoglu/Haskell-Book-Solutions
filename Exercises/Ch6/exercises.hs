-- Chapter Exercises
-- Does it typecheck?
-- 1
data Person = 
    Person Bool
    deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)  

--  2
data Mood = Blah 
          | Woot deriving (Show, Eq) 
settleDown x = if x == Woot
                  then Blah 
                  else x 

-- 4 
type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)
s1 = Sentence "dogs" "drool" "asd"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?
data Rocks =
    Rocks String 
    deriving (Eq, Show)
data Yeah =
    Yeah Bool 
    deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- Match the types
-- 1
i :: Num a => a
i = 1

-- 2, 3
f ::  Fractional a => a
f = 1.0 

-- 4
g :: RealFrac a => a
g = 1.0

-- 5
freud :: Int -> Int
freud x = x

-- 6
myY = 1 :: Int
sigmund :: Int -> Int
sigmund x = myY

-- 7
myX = 1 :: Int
sigmund' :: Int -> Int
sigmund' x = myX