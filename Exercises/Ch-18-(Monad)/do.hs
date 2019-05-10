
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

