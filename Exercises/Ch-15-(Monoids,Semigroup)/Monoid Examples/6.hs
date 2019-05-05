import Test.QuickCheck
import Data.Semigroup

data BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _                             = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = do
        a <- elements [True, False]
        return $ BoolConj a 

semigroupBool :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupBool a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq a, Monoid a => a -> Bool)
monoidLeftIdentity a = mappend a mempty == a 

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = mappend mappend a == a 

type SemiAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO ()
main = do
    quickCheck (semigroupBool :: SemiAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool )
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

