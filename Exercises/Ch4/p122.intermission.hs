--Intermission: Exercise page 122
module Mood where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood

changeMood Blah = Woot
changeMood    _ = Blah