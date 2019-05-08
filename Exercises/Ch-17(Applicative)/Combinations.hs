-- Combinations Exercises
import Control.Applicative (liftA3)

stops :: String 
vowels :: String 

stops = "pbtdkg" 
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)] 
combos = liftA3 (,,) 
-- What the LiftA3 is doing here is that
-- (,,) <$> "a" <*> "b" <*> "c"

