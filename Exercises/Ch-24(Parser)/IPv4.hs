-- parser for IPv4 adresses
import Data.Word
import Text.Trifecta

data IPAddress = IPAddress Word32 deriving (Eq, Ord)

digitRange :: Integer -> Integer -> Parser Integer
digitRange max min = do
    n <- integer
    let result 
            | n > max = fail "integer is too large"
            | n < min = fail "integer is too small"
            | otherwise = return n
    result

parseInteger :: Parser Integer
parseInteger = digitRange 999 0

parseDot :: Parser Char
parseDot = char '.'

parseIP :: Parser IPAddress 
parseIP = do
    first <- parseInteger
    _ <- parseDot
    second <- parseInteger
    _ <- parseDot
    third <- parseInteger
    _ <- parseDot
    fourth <- parseInteger
    return $ IPAddress . fromIntegral $ ((first * 256^3) + (second * 256^2) + (third * 256) + fourth)

testIP :: String -> Result IPAddress
testIP = parseString parseIP mempty

