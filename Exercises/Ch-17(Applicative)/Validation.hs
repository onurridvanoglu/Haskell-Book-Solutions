module Validation where

data Validation err a = Failure err | Success a deriving (Eq, Show) 

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a) = Success a


data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

success = Success (+1) <*> Success 1
success == Success 2 

failure = Success (+1) <*> Failure [StackOverflow]
failure == Failure [StackOverflow]

failure'= Failure [StackOverflow] <*> Success (+1)
failure' == Failure [StackOverflow] 

failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]
failures == Failure [MooglesChewedWires, StackOverflow]
