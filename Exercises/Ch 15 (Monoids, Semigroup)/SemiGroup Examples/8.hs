data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Fst x) <> (Fst y) = Fst x
    _ <> (Snd y)       = Snd y
    (Snd x) <> _       = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a, return $ Snd b]
    
semigroupOr :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupOr a b c = (a <> (b <> c)) == ((a <> b) <> c)

type OrStr = Or String String

type OrAssoc = OrStr -> OrStr -> OrStr -> Bool

main :: IO()
main = do
    quickCheck (semigroupOr :: OrAssoc)
    