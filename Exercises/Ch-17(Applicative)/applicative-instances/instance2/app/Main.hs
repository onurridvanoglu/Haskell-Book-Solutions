module Main where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance Functor f => Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Applicative

