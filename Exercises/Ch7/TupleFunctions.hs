-- Pattern Matching Tuples
module TupleFunctions where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y
    
addEmUp2Alt :: Num a => (a, a) -> a 
addEmUp2Alt tup = (fst tup) + (snd tup)
    
fst3 :: (a, b, c) -> a 
fst3 (x, _, _) = x
    
third3 :: (a, b, c) -> c
third3 (_, _, x) = x

fst4 :: (a, b, c, d) -> a 
fst4 (x, _, _, _) = x
snd4 :: (a, b, c, d) -> b 
snd4 (_, x, _, _) = x
third4 :: (a, b, c, d) -> c 
third4 (_, _, x, _) = x
fourth4 :: (a, b, c, d) -> d
fourth4 (_, _, _, x) = x

-- Intermission: Exercises 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, y, z) (g, f, h) = ((x, g), (z, h))