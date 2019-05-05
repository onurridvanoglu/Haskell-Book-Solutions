module Jammin where

data Fruit = 
    Peach 
    |Plum
    |Apple
    |Blackberry
    deriving (Eq, Ord, Show)

{- data JamJars = 
    Jam Fruit Int 
    deriving (Eq, Show) -}

data JamJars =
        Jam { fruit :: Fruit 
            , quantity :: Int }
            deriving (Eq, Ord, Show)
row1 = Jam Peach 7
row2 = Jam Plum 5
row3 = Jam Blackberry 12
row4 = Jam Apple  15
row5 = Jam Peach 9
row6 = Jam Blackberry 3
allJam = [row1, row2, row3, row4, row5, row6]

allNumbers :: [JamJars] -> Int
allNumbers = sum . (map quantity)

compareCanned :: JamJars -> JamJars -> Ordering
compareCanned (Jam _ n) (Jam _ n') = compare n n'

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

equalKind :: JamJars -> JamJars -> Bool
equalKind (Jam k _) (Jam k' _) = k == k'


