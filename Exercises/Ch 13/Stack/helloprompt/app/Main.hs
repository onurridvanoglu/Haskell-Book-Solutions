module Main where

import System.IO

import Lib

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Please enter your name: "
    name <- getLine
    sayHello name


