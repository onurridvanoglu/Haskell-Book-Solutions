module Cipher where

import Data.Char

caesar :: String -> String
caesar [] = []
caesar (x:xs)   
        |(mod (ord x + 1) 97) > 25 = (chr $ (mod (ord x + 1) 97) + 71) : caesar xs
        |otherwise = (chr $ (+1) $ ord x) : caesar xs 
     

{-caesar (x:xs) = case (mod (ord x + 3) 97) > 25 of
        True  -> b : caesar xs
                where   
                        b = (chr $ (mod (ord x + 3) 97) + 71)
        False -> (chr $ (+3) $ ord x) : caesar xs -}