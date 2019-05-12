-- function composition with monadic structure

import Control.Monad ((>=>))

sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askforAge :: IO Int
askforAge = getAge "Hello! How old are you? "