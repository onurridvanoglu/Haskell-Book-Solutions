data Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary 
        return $ Identity a

semigroupId :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupId a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
    quickCheck (semigroupId :: IdentityAssoc)

