import Test.QuickCheck
import Data.Semigroup

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two (a b) (<>) Two (a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b 

semigroupAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = mappend a mempty == a 

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = mappend mempty a == a 

type TwoStr = Two String String
type semiAssoc = TwoStr -> TwoStr -> TwoStr -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: semiAssoc)
    quickCheck (monoidLeftIdentity :: TwoStr -> Bool)
    quickCheck (monoidRightIdentity:: TwoStr -> Bool)