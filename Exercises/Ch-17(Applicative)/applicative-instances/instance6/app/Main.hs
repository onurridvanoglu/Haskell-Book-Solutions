module Main where

import Control.Applicative
import Test.QuickCheck

data Four' a b = Four' a a a b

instance (Semigroup a, Semigroup b) => Semigroup (Four' a b) where
    (Four' a b c d) <> (Four' a' b' c' d') = Four' (a <> a') (b <> b') (c <> c') (d <> d')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Four' a b) where
    mempty = Four' mempty mempty mempty mempty
    mappend = (<>)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
    pure a = Four mempty mempty mempty a
    (Four' a b c f) <*> (Four' a' b' c' d) = Four' (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d


