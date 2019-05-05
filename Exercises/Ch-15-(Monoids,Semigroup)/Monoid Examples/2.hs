import Test.QuickCheck
import Data.Semigroup

data Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

semigroupAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = mappend a mempty == a 

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = mappend mempty a == a 

type IdStr = Identity String
type IdAssoc = IdStr -> IdStr -> IdStr -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (monoidLeftIdentity :: IdStr -> Bool)
    quickCheck (monoidRightIdentity :: IdStr -> Bool)