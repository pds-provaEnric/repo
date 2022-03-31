data Pal = Oros | Copes | Espases | Bastos deriving (Show,Eq)
data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla | Sota | Cavall | Rei deriving (Show,Eq)
data Carta = Cart TipusCarta Pal deriving Show
data Trumfu = Trumf Pal deriving Show

-- https://www.ludoteka.com/juegos/butifarra

totsPal :: [Pal]
totsPal = [Oros,Copes,Espases,Bastos]
totsTipus :: [TipusCarta]
totsTipus = [As,Dos,Tres,Quatre,Cinc,Sis,Set,Vuit,Manilla,Sota,Cavall,Rei]

totesCartes :: [[Carta]]
totesCartes = [[Cart As Oros,Cart Dos Oros,Cart Tres Oros,Cart Quatre Oros,Cart Cinc Oros,Cart Sis Oros,Cart Set Oros,Cart Vuit Oros,Cart Manilla Oros,Cart Sota Oros,Cart Cavall Oros,Cart Rei Oros],
    [Cart As Copes,Cart Dos Copes,Cart Tres Copes,Cart Quatre Copes,Cart Cinc Copes,Cart Sis Copes,Cart Set Copes,Cart Vuit Copes,Cart Manilla Copes,Cart Sota Copes,Cart Cavall Copes,Cart Rei Copes],
    [Cart As Espases,Cart Dos Espases,Cart Tres Espases,Cart Quatre Espases,Cart Cinc Espases,Cart Sis Espases,Cart Set Espases,Cart Vuit Espases,Cart Manilla Espases,Cart Sota Espases,Cart Cavall Espases,Cart Rei Espases],
    [Cart As Bastos,Cart Dos Bastos,Cart Tres Bastos,Cart Quatre Bastos,Cart Cinc Bastos,Cart Sis Bastos,Cart Set Bastos,Cart Vuit Bastos,Cart Manilla Bastos,Cart Sota Bastos,Cart Cavall Bastos,Cart Rei Bastos]]

aa :: [Carta]
aa = [Cart As Oros,Cart Dos Oros,Cart Tres Oros,Cart Quatre Oros,Cart Cinc Oros,Cart Sis Oros,Cart Set Oros,Cart Vuit Oros,Cart Manilla Oros,Cart Sota Oros,Cart Cavall Oros,Cart Rei Oros,
    Cart As Copes,Cart Dos Copes,Cart Tres Copes,Cart Quatre Copes,Cart Cinc Copes,Cart Sis Copes,Cart Set Copes,Cart Vuit Copes,Cart Manilla Copes,Cart Sota Copes,Cart Cavall Copes,Cart Rei Copes,
    Cart As Espases,Cart Dos Espases,Cart Tres Espases,Cart Quatre Espases,Cart Cinc Espases,Cart Sis Espases,Cart Set Espases,Cart Vuit Espases,Cart Manilla Espases,Cart Sota Espases,Cart Cavall Espases,Cart Rei Espases,
    Cart As Bastos,Cart Dos Bastos,Cart Tres Bastos,Cart Quatre Bastos,Cart Cinc Bastos,Cart Sis Bastos,Cart Set Bastos,Cart Vuit Bastos,Cart Manilla Bastos,Cart Sota Bastos,Cart Cavall Bastos,Cart Rei Bastos]


bbbb :: Pal -> [TipusCarta] -> [Carta]
bbbb x [] = []
bbbb x (y:ys) = (Cart y x):(bbbb x ys)


aaaa :: [Pal] -> [TipusCarta] -> [[Carta]]
aaaa [] _ = []
aaaa (x:xs) ys = (bbbb x ys):(aaaa xs ys)

--- >>> aaaa totsPal totsTipus
-- [[Cart As Oros,Cart Dos Oros,Cart Tres Oros,Cart Quatre Oros,Cart Cinc Oros,Cart Sis Oros,Cart Set Oros,Cart Vuit Oros,Cart Manilla Oros,Cart Sota Oros,Cart Cavall Oros,Cart Rei Oros],[Cart As Copes,Cart Dos Copes,Cart Tres Copes,Cart Quatre Copes,Cart Cinc Copes,Cart Sis Copes,Cart Set Copes,Cart Vuit Copes,Cart Manilla Copes,Cart Sota Copes,Cart Cavall Copes,Cart Rei Copes],[Cart As Espases,Cart Dos Espases,Cart Tres Espases,Cart Quatre Espases,Cart Cinc Espases,Cart Sis Espases,Cart Set Espases,Cart Vuit Espases,Cart Manilla Espases,Cart Sota Espases,Cart Cavall Espases,Cart Rei Espases],[Cart As Bastos,Cart Dos Bastos,Cart Tres Bastos,Cart Quatre Bastos,Cart Cinc Bastos,Cart Sis Bastos,Cart Set Bastos,Cart Vuit Bastos,Cart Manilla Bastos,Cart Sota Bastos,Cart Cavall Bastos,Cart Rei Bastos]]

--- >>> totesCartes
-- [[Cart As Oros,Cart Dos Oros,Cart Tres Oros,Cart Quatre Oros,Cart Cinc Oros,Cart Sis Oros,Cart Set Oros,Cart Vuit Oros,Cart Manilla Oros,Cart Sota Oros,Cart Cavall Oros,Cart Rei Oros],[Cart As Copes,Cart Dos Copes,Cart Tres Copes,Cart Quatre Copes,Cart Cinc Copes,Cart Sis Copes,Cart Set Copes,Cart Vuit Copes,Cart Manilla Copes,Cart Sota Copes,Cart Cavall Copes,Cart Rei Copes],[Cart As Espases,Cart Dos Espases,Cart Tres Espases,Cart Quatre Espases,Cart Cinc Espases,Cart Sis Espases,Cart Set Espases,Cart Vuit Espases,Cart Manilla Espases,Cart Sota Espases,Cart Cavall Espases,Cart Rei Espases],[Cart As Bastos,Cart Dos Bastos,Cart Tres Bastos,Cart Quatre Bastos,Cart Cinc Bastos,Cart Sis Bastos,Cart Set Bastos,Cart Vuit Bastos,Cart Manilla Bastos,Cart Sota Bastos,Cart Cavall Bastos,Cart Rei Bastos]]


b :: Trumfu
b = Trumf Oros

a :: [Carta]
a = [Cart Manilla Oros,Cart Sis Oros,Cart As Oros]

instance Eq Carta where
    Cart x z== Cart y s= x == y && z == s

instance Ord TipusCarta where
    -- Manilla
    _ <= Manilla = True
    Manilla <= _ = False
    -- As
    _ <= As = True
    As <= _ = False
    -- Rei
    _ <= Rei = True
    Rei <= _ = False
    -- Cavall
    _ <= Cavall = True
    Cavall <= _ = False
   -- Sota 
    _ <= Sota = True
    Sota <= _ = False
    -- Vuit
    _ <= Vuit = True
    Vuit <= _ = False
    -- Set
    _ <= Set = True
    Set <= _ = False
    -- Sis
    _ <= Sis = True
    Sis <= _ = False
    -- Cinc
    _ <= Cinc = True
    Cinc <= _ = False
    -- Quatre
    _ <= Quatre = True
    Quatre <= _ = False
    -- Tres
    _ <= Tres = True
    Tres <= _ = False
    -- Dos
    _ <= Dos = True

instance Ord Carta where
    Cart x _<= Cart y _= x <= y
-- -- -- -- -- -- -- --
-- FUNCIONS PRINCIPALS
-- -- -- -- -- -- -- -- 

-- CONTAR ELS PUNTS D'UNA LLISTA DE CARTES
punts :: [Carta] -> Int
punts [] = 0
punts ((Cart Manilla _):xs) = 5 + punts xs
punts ((Cart As _):xs) = 4 + punts xs
punts ((Cart Rei _):xs) = 3 + punts xs
punts ((Cart Cavall _):xs) = 2 + punts xs
punts ((Cart Sota _):xs) = 1 + punts xs
punts (_:xs) = punts xs

--- >>> punts a
-- 9


-- RETORNA UNA TUPLA AMB LES CARTES GUANYADES PER CADA EQUIP
--cartesGuanyades:: Trumfu -> [Carta] -> Int -> ([Carta],[Carta])


-- -- -- -- -- -- -- --
-- FUNCIONS AUXILIARS
-- -- -- -- -- -- -- -- 

-- RETORNA LLISTA DE CARTES QUE CONCORDEN AMB EL PAL
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal [] _ = []
cartesPal ((Cart x y):xs) z
 | y == z = (Cart x y):(cartesPal xs z)
 | otherwise = (cartesPal xs z)

--- >>> cartesPal aa Oros
-- [Cart As Oros,Cart Dos Oros,Cart Tres Oros,Cart Quatre Oros,Cart Cinc Oros,Cart Sis Oros,Cart Set Oros,Cart Vuit Oros,Cart Manilla Oros,Cart Sota Oros,Cart Cavall Oros,Cart Rei Oros]


-- RETORNA EL PAL GUANYADOR DE LA LLISTA DE CARTES
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa ((Cart x z):xs) (Trumf y)
 | (cartesPal ((Cart x z):xs) y) == [] = z
 | otherwise = y


--- >>> palGuanyadorBasa [Cart Sis Oros,Cart Tres Bastos,Cart Manilla Oros,Cart Set Oros] (Trumf Bastos)
-- Bastos

-- RETORNA LA CARTA MES ALTA DE LA LLISTA
mesAlt :: [Carta] -> Carta
mesAlt [x] = x 
mesAlt (x:xs)
 | x > (mesAlt xs) = x
 | otherwise = mesAlt xs


-- RETORNA LA POSICIO DE LA CARTA A LA LLISTA
posCarta :: [Carta] -> Carta -> Int
posCarta (x:xs) y
 | x /= y = 1 + (posCarta xs y)
 | otherwise = 1

-- RETORNA LA POSICIO DE LA CARTA GUANYADORA
quiGuanya :: [Carta] -> Trumfu -> (Carta,Int)
quiGuanya x (Trumf y)= ((mesAlt (cartesPal x (palGuanyadorBasa x (Trumf y)))),(posCarta x (mesAlt (cartesPal x (palGuanyadorBasa x (Trumf y))))))

--- >>> quiGuanya [Cart Dos Espases,Cart Dos Oros,Cart Dos Bastos,Cart Dos Copes] (Trumf Bastos)
-- (Cart Dos Bastos,3)


-- RETORNA LA PERSONA QUE COMENÇARA LA SEGÜENT BASA
quiSortira :: Int -> Int -> Int
quiSortira x y
 | x + y - 1 <= 4 = x + y - 1
 | otherwise = x + y - 5
-- el -1 és perque el primer a tirar es el mateix que tira, per tant si guanya el 2 nomes ha de moure 1

--- >>> quiSortira 2 4
-- 1


jugades :: [Carta] -> Pal -> [Carta] -> [Carta]
jugades x _ _ = x




