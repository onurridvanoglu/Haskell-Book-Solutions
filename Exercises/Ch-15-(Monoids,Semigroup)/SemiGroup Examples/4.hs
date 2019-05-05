data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')
    
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

semigroupThree :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupThree a b c = (a <> (b <> c)) == ((a <> b) <> c)

type ThreeStrings = Three String String String

type ThreeAssoc = ThreeStrings -> ThreeStrings -> ThreeStrings -> Bool

main :: IO ()
main = do
    quickCheck (semigroupThree :: ThreeAssoc)
    