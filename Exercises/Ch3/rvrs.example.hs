--Exercise 5
module Reverse where

s = "Curry is awesome"
rvrs = (drop 9 s) ++ (drop 5(take 9 s)) ++ take 5 s