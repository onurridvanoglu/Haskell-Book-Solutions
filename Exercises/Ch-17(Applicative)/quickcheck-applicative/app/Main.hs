module BadMonoid where



import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes 
        
data Bull = Fools | Twoo deriving (Eq, Show)
        
instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]
        
instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools
        
        -- EqProp is a class available in Checkers
        -- Checkers requires an instance of EqProp in order for us to use monoid
instance EqProp Bull where
    (=-=) = eq
        
main :: IO ()
        -- passing a value of our type to monoid indicates to monoid which Arbitrary
        -- instance to use to get random values
main = quickBatch (monoid Twoo)