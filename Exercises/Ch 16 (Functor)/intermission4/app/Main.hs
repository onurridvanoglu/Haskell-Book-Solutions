module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c 

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f 

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =(fmap g (fmap f x)) == (fmap (g . f) x)

type ThreeStr = Three String String String 
type FunStr = Fun String String 
type ThreeCompose = FunStr -> FunStr -> ThreeStr -> Bool

main :: IO ()
main = do 
    quickCheck (functorIdentity :: ThreeStr -> Bool)
    quickCheck (functorCompose' :: ThreeCompose)