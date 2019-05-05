data BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _ <> (BoolDisj True)                 = BoolDisj True
    (BoolDisj True) <> _                 = BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- elements [True, False]
        return $ BoolConj a 

semigroupBool :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupBool a b c = (a <> (b <> c)) == ((a <> b) <> c)

type SemiDisJ = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main = do 
    quickCheck (semigroupBool :: SemiDisJ)