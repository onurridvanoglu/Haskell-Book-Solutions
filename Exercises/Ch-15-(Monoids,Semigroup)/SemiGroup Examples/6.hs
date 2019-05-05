data BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _                             = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = do
        a <- elements [True, False]
        return $ BoolConj a 

semigroupBool :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupBool a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO ()
main = do
    quickCheck (semigroupBool :: BoolAssoc)