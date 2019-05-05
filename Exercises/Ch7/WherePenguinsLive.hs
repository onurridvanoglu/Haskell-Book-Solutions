--Wherepenguinslive.hs
module WherePenguinsLive where 

data WherePenguinsLive =
    Galapagos
   |Antarctic
   |Australia
   |SouthAfrica
   |SouthAmerica
    deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive
    deriving (Eq, Show)

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False
antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctic) = True
antarcticPenguin _ = False

{- in this final function, the || operator
 is an `or` function, which will return True
 if either value is True -}

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
    (galapagosPenguin p) || (antarcticPenguin p)

