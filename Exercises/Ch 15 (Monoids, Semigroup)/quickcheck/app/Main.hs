module Main where

import Data.Monoid
import Test.QuickCheck

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a
    
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a
    
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do 
    quickCheck (monoidAssoc :: String -> String -> String -> Bool)
    quickCheck (monoidLeftIdentity :: String -> Bool)
    quickCheck (monoidRightIdentity :: String -> Bool)