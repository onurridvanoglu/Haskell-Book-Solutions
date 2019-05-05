myHead :: Num a => [a] -> a
myHead [] = 0
myHead (x : _) = x

myTail :: [a] -> [a]  
myTail [] = []
myTail (_ : xs) = xs

--Exercise
myEnumFromTo :: (Enum a, Ord a) => a -> a -> [a]
myEnumFromTo x y 
            | x <= y = x : myEnumFromTo (succ x) y
            | otherwise = []
{- Another possible way to write myEnumFromTo 
   But this does not satisfy the strings cuz of the
   (x + 1) function doesnt apply to the chars     -}
myEnumFromTo1 :: (Num a, Ord a) => a -> a -> [a]
myEnumFromTo1 x y 
            | x <= y = x : myEnumFromTo1 (x + 1) y
            | otherwise = []

--Intermission: Exercise
myWords :: [Char] -> [[Char]]
myWords []       = []
myWords (' ':xs) = myWords xs
myWords s        = w : myWords t
  where
    w = takeWhile (/= ' ') s
    t = dropWhile (/= ' ') s

-- myLines (same thing in the myWords but only diffrence is '\n')
firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: [Char] -> [[Char]]
myLines [] = []
myLines ('\n':xs) = myLines xs
myLines x = a : myLines b
    where
        a = takeWhile (/= '\n') x
        b = dropWhile (/= '\n') x

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]
    
main :: IO ()
main =
    print $ "Are they equal? "
        ++ show (myLines sentences == shouldEqual)

f :: String -> String
f xs = [x | x <- xs, elem x ['A'..'Z']]

