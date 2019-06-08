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

type NumberingPlanArea = Integer -- AKA area code
type Exchange = Integer
type LineNumber = Integer

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

-- Writing a function that only parses three digit integers
parseThreeIntegers :: Parser Integer
parseThreeIntegers = digitRange 999 100

-- A function to parse four digit integers (for linenumber)
parseFourIntegers :: Parser Integer
parseFourIntegers = digitRange 9999 1000

digitRange :: Integer -> Integer -> Parser Integer
digitRange max min = do
    n <- integer
    let result
            | n > max = fail "integer is too large"
            | n < min = fail "integer is too small"
            | otherwise = return n
    result

-- A funciton to neglate unwanted characters (space and dash)
parseDash :: Parser Char
parseDash = char '-'

parseSpace :: Parser Char
parseSpace = char ' '

-- This is not working?
parseUnwanted :: Parser Char
parseUnwanted = (char '-') <|> (char ' ')

-- A function to neglate paranthesis in the area code
-- (*>) :: f a -> f b -> f b 
-- Sequence actions, discarding the value of the first argument.
parseParanthesis :: Parser Integer
parseParanthesis = char '(' *> parseThreeIntegers <* char ')'

-- Works!!
parsePhoneDashed :: Parser PhoneNumber
parsePhoneDashed = do
    area <- parseThreeIntegers
    _ <- parseDash
    exchange <- parseThreeIntegers
    _ <- parseDash
    lineno <- parseFourIntegers
    return $ PhoneNumber area exchange lineno

parsePhonePara :: Parser PhoneNumber
parsePhonePara = do
    area <- parseParanthesis
    _ <- parseSpace
    exchange <- parseThreeIntegers
    _ <- parseDash
    lineno <- parseFourIntegers
    return $ PhoneNumber area exchange lineno

-- Works now but why the older version is not working?
parsePhoneSpace :: Parser PhoneNumber
parsePhoneSpace = do
    area <- parseThreeIntegers
    exchange <- parseThreeIntegers
    lineno <- parseFourIntegers
    return $ PhoneNumber area exchange lineno

-- count :: Applicative m => Int -> m a -> m [a]
-- count n p parses n occurrences of p in sequence. A list of results is returned.
-- So using read functions give the type Integer
parsePhoneNoSpace :: Parser PhoneNumber
parsePhoneNoSpace = do
    area <- count 3 digit
    exchange <- count 3 digit
    lineno <- count 4 digit
    return $ PhoneNumber (read area) (read exchange) (read lineno)


parsePhone :: Parser PhoneNumber
parsePhone = (try parsePhoneDashed) 
         <|> (try parsePhonePara) 
         <|> (try parsePhoneNoSpace) 
         <|> (try parsePhoneSpace)

-- data Result a = Error String | Success a
-- result of running a parser
testParser :: String -> Result PhoneNumber
testParser = parseString parsePhone mempty