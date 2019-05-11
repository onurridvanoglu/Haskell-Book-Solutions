
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

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "Whats ur name? "
    name <- getLine
    putStrLn ("y hello thar " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' = do
    putStrLn "What ur name?" >> getLine >>= 
                                \name -> putStrLn ("hello thar ya facking " ++ name)


twoBinds :: IO ()
twoBinds = do
    putStrLn "What's ur name?"
    name <- getLine
    putStrLn "How old are you?"
    age <- getLine
    putStrLn ("Hello thar ya facking: " ++ name ++ "who is: " ++ age ++ "years old.")

twoBinds' :: IO () 
twoBinds' = 
    putStrLn "What's ur name? " >> getLine >>= \name ->
        putStrLn "How old are you?" >> getLine >>= \age -> putStrLn ("hello thar ya facking " ++ name ++ "who is " ++ age ++  " years old")

twoBinds'' :: IO () 
twoBinds'' =
    putStrLn "name pls:" >> 
    getLine >>=
        (\name ->
            putStrLn "age pls:" >> getLine >>=
            (\age ->
                putStrLn ("y helo thar: " ++name++"whois:"
                                          ++ age ++ " years old.")))


