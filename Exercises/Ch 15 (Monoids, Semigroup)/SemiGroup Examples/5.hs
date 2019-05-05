data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    Four (x y z h) <> Four (x' y' z' h') = Four (x <> x') (y <> y') (z <> z') (h <> h')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

semigroupFour :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupFour a b c = (a <> (b <> c)) == ((a <> b) <> c)

type FourStr = Four String String String String

type FourAssoc = FourStr -> FourStr -> FourStr -> Bool

main :: IO ()
main = do 
    quickCheck (semigroupFour :: FourAssoc)