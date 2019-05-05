import System.IO


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                    | AgeTooLow
                    | PersonInvalidUnknown String
                    deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
                        "Name was: " ++ show name ++
                        " Age was: " ++ show age

gimmePerson :: IO()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStr "Please enter your name: "
    name <- getLine
    putStr "Please enter your age: "
    age <- getLine
    let person = mkPerson name (read age :: Integer)
  case person of
    (Right p) -> do
      putStrLn "Yay! "
      putStrLn $ show p
    (Left p) -> do
      putStrLn "error: "
      putStrLn $ show p
