module Main where

import Control.Applicative
import Test.QuickCheck

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Three' a b) where
    (Three' a b c) <> (Three' a' b' c') = Three' (a <> a') (b <> b') (c <> c')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Three' a b) where
    mempty = Three' mempty mempty mempty 
    mappend = (<>)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
    pure a = Three' mempty mempty a
    (Three' a f g) <*> (Three' b c d) = Three' (a <> b) (f c) (g d)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

