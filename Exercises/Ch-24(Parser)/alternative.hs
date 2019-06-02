{-# LANGUAGE QuasiQuotes #-}
module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ
import Data.Ratio ((%))

type NumberOrString = Either Integer String 

a = "lol" 
b = "1453"
c = "1453lol1453"

parseNos :: Parser NumberOrString
parseNos = 
    skipMany (oneOf "\n")
    >>
    (Left <$> integer) <|> (Right <$> some letter)

main :: IO ()
main = do
    print $ parseString (some letter) mempty a
    print $ parseString integer mempty b
    print $ parseString parseNos mempty a
    print $ parseString parseNos mempty b
    print $ parseString (many parseNos) mempty c
    print $ parseString (some parseNos) mempty c
    print $ parseString parseNos mempty eitherOr

eitherOr :: String
eitherOr = [r|
123
abc
456
def
baris|]

-- Intermission: Exercise

type RationalOrInteger = Either Rational Integer

x = "1/2" 
y = "2/1"
z = "1/2231"
g = "1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denumerator <- decimal
    return (numerator % denumerator)

parseDecimal :: Parser Integer
parseDecimal = decimal
    
parseFDs :: Parser RationalOrInteger
parseFDs = 
    skipMany (oneOf "\n")
    >>
    (Left <$> try parseFraction) <|> (Right <$> parseDecimal)

main' :: IO ()
main' = do
    print $ parseString parseFDs mempty x
    print $ parseString parseFDs mempty y
    print $ parseString parseFDs mempty z
    print $ parseString parseFDs mempty g