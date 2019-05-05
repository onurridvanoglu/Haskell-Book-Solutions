module Main where

import Test.QuickCheck

data Identity a = Identity a deriving (Eq , Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

functorIdentity :: (Functor f, Eq (f a)) => (a -> b) -> (b -> c) -> f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (f . g) x)

type IdentityStr = Identity String 
type IdCompose = IdentityStr -> IdentityStr -> IdentityStr -> Bool

main :: IO ()
main = do
    quickCheck (functorIdentity :: Identity a -> Bool)
    quickCheck (functorCompose :: IdCompose)



