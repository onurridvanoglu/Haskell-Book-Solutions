module Main where

import Control.Applicative
import Test.QuickCheck

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)

instance (Semigroup a, Semigroup b, Semigroup c, Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend = (<>)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure a = Three mempty mempty a
    (Three x y f) <*> (Three a b c) = Three (x <> a) (y <> b) (f c) 

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

