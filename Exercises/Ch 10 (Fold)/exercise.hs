import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                    deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)), DbString "Hello, world!", DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]