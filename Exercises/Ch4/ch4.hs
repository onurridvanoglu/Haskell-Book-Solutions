-- greetIfCool1.hs
greetIfCool1 :: String -> IO()
greetIfCool1 coolness = 
    if cool 
        then putStrLn "eyyyyyy. What's shakin' ma man?"
    else 
        putStrLn "psshhhhh"
    where cool = coolness == "downright frosty yo"



    -- greetIfCool2.hs
greetIfCool2 :: String -> IO()
greetIfCool2 coolness =
    if cool coolness  
        then putStrLn "eyyyyyy, whats shaking ma man?"
    else
        putStrLn "what da fuckk!?!"
    where cool a = a == "sup ma man?"


-- Palindromefunction.hs
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

--Absolutefunction.hs
myAbs :: Integer -> Integer
myAbs x = 
    if x<0
        then negate x
    else 
        x

--tupleExercise
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

--syntax correction
g xs = w + 1
    where w = length xs

