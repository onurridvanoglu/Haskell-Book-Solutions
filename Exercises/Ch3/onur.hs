letterIndex :: Int -> Char
letterIndex x = "Onur ben senin aq" !! x

myReverse :: String -> String
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]


