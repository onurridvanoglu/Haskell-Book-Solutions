module Main where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

instance Semigroup a => Semigroup (Pair a) where
    (Pair a b) <> (Pair a' b') = Pair (a <> a) (b <> b')

instance (Semigroup a, Monoid a) => Monoid (Pair a) where
    mempty = Pair mempty mempty
    mappend = (<>)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure a = Pair a a
    (Pair f g) <*> (Pair a' b') = Pair (f a') (g b')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary
        return $ Pair a b



