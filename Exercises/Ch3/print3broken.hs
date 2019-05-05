--print3broken.hs
module Print3Broken where


printSecond :: IO ()
printSecond = do
    putStrLn "Yarrrr"
main :: IO ()
main = do 
    putStrLn greeting
    printSecond 
    where greeting = "Yarrrr"