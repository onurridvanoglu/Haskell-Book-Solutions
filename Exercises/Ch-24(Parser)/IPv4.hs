-- parser for IPv4 adresses
import Data.Word
import Text.Trifecta

data IPAddress = IPAddress Word32 deriving (Eq, Show, Ord)

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
    f <- parseInteger
    _ <- parseDot
    s <- parseInteger
    _ <- parseDot
    t <- parseInteger
    _ <- parseDot
    four <- parseInteger
    return $ IPAddress . fromIntegral $ ((f * 256^3) + (s * 256^2) + (t * 256) + four)

testIP :: String -> Result IPAddress
testIP = parseString parseIP mempty

