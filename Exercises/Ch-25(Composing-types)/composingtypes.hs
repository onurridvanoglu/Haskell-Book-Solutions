-- Identity 
newtype Identity a = Identity { runIdentity :: a }

--Compose 
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

