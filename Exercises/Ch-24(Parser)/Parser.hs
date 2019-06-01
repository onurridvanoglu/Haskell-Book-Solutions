import Text.Trifecta           (Parser, unexpected, char, parseString, choice, string, integer)
import Text.Parser.Combinators (eof)
import Control.Applicative     ((<|>))
stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop
allThree :: Parser String
allThree = do 
    n <- (show <$> integer)
    _ <- eof
    return n

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'

-- Intermission: Exercises
testParse' :: Parser () -> IO ()
testParse' p = print $ parseString p mempty "123"

oneEOF = one >> eof
oneTwoEOF = oneTwo >> eof

test1 = do
    pNL "oneEOF:"
    testParse' oneEOF
    pNL "oneTwo:"
    testParse' oneTwoEOF

