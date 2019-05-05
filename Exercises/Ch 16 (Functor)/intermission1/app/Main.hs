module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
    
data Identity a = Identity a deriving (Eq , Show)
    
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
    
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a
    
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f
        
functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)
        
functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

type FunStr = Fun String String
type IdCompose = FunStr -> FunStr -> Identity String -> Bool
    
main :: IO ()
main = do
    quickCheck (functorIdentity :: Identity String -> Bool)
    quickCheck (functorCompose' :: IdCompose)