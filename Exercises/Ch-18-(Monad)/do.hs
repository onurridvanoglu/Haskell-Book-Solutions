
import Control.Applicative ((*>))

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO ()
sequencing' = do
    putStrLn "blah" >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' = do
    putStrLn "blah" *> putStrLn "another thing"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' = do
    getLine >>= putStrLn



