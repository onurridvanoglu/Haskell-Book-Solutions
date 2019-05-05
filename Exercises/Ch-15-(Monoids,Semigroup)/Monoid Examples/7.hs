import Test.QuickCheck
import Data.Semigroup

data BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> _ = BoolDisj True
    _ <> (BoolDisj True) = BoolDisj True 
    _ <> _               = BoolDisj False

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- elements [True, False]
        return $ BoolDisj a 

semigroupBool :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupBool a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq a, Monoid a => a -> Bool)
monoidLeftIdentity a = mappend a mempty == a 

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = mappend mappend a == a 

type SemiAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main = do
    quickCheck (semigroupBool :: SemiAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool )
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)