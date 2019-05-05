module Main where

import Control.Monad
import System.IO
import System.Exit (exitSuccess)
import Lib
import Data.Char

main :: IO ()
main = forever $ do
    hSetBuffering stdout NoBuffering
    putStr "Please enter the word: "
    line1 <- getLine
    case ((f line1) == reverse (f line1)) of
        True -> putStrLn "It's a palindrome!"
        False -> putStrLn "Nope!"
    exitSuccess


f :: String -> String 
f [] =  []
f (x:xs) = toLower x : f xs
