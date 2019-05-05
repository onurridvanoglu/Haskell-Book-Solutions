module Lib
    ( someFunc
    ) where
import Test.QuickCheck

someFunc :: IO ()
someFunc = putStrLn "someFunc"

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']
