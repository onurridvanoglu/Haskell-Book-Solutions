data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two z t) = Two (x <> z) (y <> t)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary 
        return $ Two a b

semigroupTwo :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupTwo a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TwoStrings = Two String String

type TwoAssoc = TwoStrings -> TwoStrings -> TwoStrings -> Bool

main :: IO ()
main = do 
    quickCheck (semigroupTwo :: TwoAssoc)
