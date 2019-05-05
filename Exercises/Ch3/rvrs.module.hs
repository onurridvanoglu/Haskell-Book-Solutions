-- Exercise 6 from page 116
module Reverse where

rvrs :: String -> String
rvrs x = (drop 9 x) ++ (drop 5(take 9 x)) ++ take 5 x

main :: IO()
main = print (rvrs "Curry is awesome")