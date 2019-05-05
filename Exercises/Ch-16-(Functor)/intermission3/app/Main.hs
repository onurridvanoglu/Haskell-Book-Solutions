module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary 
        return $ Two a b

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

type TwoStr = Two String String
type FunStr = Fun String String
type TwoCompose = FunStr -> FunStr -> TwoStr -> Bool

main :: IO ()
main = do 
    quickCheck (functorIdentity :: TwoStr -> Bool)
    quickCheck (functorCompose' :: TwoCompose)
