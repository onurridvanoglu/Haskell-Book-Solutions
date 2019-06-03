{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator can't be zero"
        _ -> return (numerator % denominator)

-- Writing a main function that will run both attoparsec and 
-- trifecta versions to compare easily

main :: IO ()
main = do
    -- parseOnly is Attoparsec
    print $ parseOnly parseFraction badFraction
    print $ parseOnly parseFraction shouldWork
    print $ parseOnly parseFraction shouldAlsoWork
    print $ parseOnly parseFraction alsoBad
    -- parseString is Trifecta
    print $ parseString parseFraction mempty badFraction
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad