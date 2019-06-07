{-# LANGUAGE OverloadedStrings #-}

import Text.Trifecta
import Control.Applicative

-- 1 (SemVer example)
data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
type Version = (Major, Minor, Patch)

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show, Ord)

skipHypenOrDot :: Parser ()
skipHypenOrDot = skipMany (char '.' <|> char '-')

parseVer :: Parser Version
parseVer = do
    major <- integer
    skipHypenOrDot
    minor <- integer
    skipHypenOrDot
    patch <- integer
    return (major, minor, patch)

stringOrNumba :: Parser NumberOrString
stringOrNumba = do
    (NOSS <$> (some letter)) <|> (NOSI <$> integer)    

parseSemVer :: Parser SemVer
parseSemVer = do
    (major, minor, patch) <- parseVer
    skipHypenOrDot
    release <- some stringOrNumba
    skipHypenOrDot
    metadata <- some stringOrNumba
    return $ SemVer major minor patch release metadata
    

-- 2 and 3
digitEx1 = "123"
digitEx2 = "abc"
integerEx1 = "123abc"
integerEx2 = "abc"
integernegative = "-123abc"

parseDigit :: Parser Char
parseDigit = digit

base10Integer :: Parser Integer
base10Integer = integer

-- 4 Phone Number

type NumberingPlanArea = Int -- AKA area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
    area <- manyTill integer (try (string "-"))
    exchange <- manyTill integer (try (string "-"))
    line <- manyTill integer (try (string "-"))
    return $ PhoneNumber area exchange line 
