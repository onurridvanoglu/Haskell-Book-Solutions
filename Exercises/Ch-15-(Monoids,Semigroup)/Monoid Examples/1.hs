data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ (<>) _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

semigroupAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = (a <> mempty) == a 

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = (mempty <> a) == a 

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)