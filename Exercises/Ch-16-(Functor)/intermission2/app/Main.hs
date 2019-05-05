module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair a where
    fmap f (Pair a b) = Pair a (f b)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b 

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

type PairCompose = Fun String String -> Fun String String -> Pair String -> Bool

main :: IO ()
main = do
    quickCheck (functorIdentity :: Pair String -> Bool)
    quickCheck (functorCompose' :: PairCompose )