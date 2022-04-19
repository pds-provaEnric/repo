import Data.Maybe (isNothing, isJust, fromJust)

data Pal = Oros | Copes | Espases | Bastos deriving (Show,Eq)
data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla | Sota | Cavall | Rei deriving (Show,Eq)
data Carta = Cart TipusCarta Pal deriving Show
data Trumfu = Trumf Pal deriving Show

cartes1 =
 [
  [Cart Manilla Bastos, Cart Vuit Bastos, Cart Tres Espases, Cart As Copes, Cart Quatre Bastos, Cart Cavall Espases, Cart Set Copes, Cart As Oros, Cart Cinc Bastos, Cart Sota Copes, Cart Quatre Espases, Cart Set Bastos],
  [Cart Sota Bastos, Cart Cavall Bastos, Cart Manilla Espases, Cart Vuit Copes, Cart Cinc Oros, Cart Vuit Espases, Cart Manilla Copes, Cart Sis Oros, Cart Sota Oros, Cart Cinc Copes, Cart Set Oros, Cart Quatre Copes],
  [Cart As Bastos, Cart Dos Oros, Cart Rei Espases, Cart Cavall Copes, Cart Vuit Oros, Cart As Espases, Cart Rei Copes, Cart Rei Oros, Cart Tres Copes, Cart Sis Copes, Cart Sis Espases, Cart Cinc Espases],
  [Cart Dos Bastos, Cart Tres Bastos, Cart Dos Espases, Cart Dos Copes, Cart Sis Bastos, Cart Set Espases, Cart Tres Oros, Cart Quatre Oros, Cart Rei Bastos, Cart Cavall Oros, Cart Manilla Oros, Cart Sota Espases]
 ]

partida1 = [
     Cart Manilla Bastos, Cart Sota Bastos, Cart As Bastos, Cart Dos Bastos,
         Cart Vuit Bastos, Cart Cavall Bastos, Cart Dos Oros, Cart Tres Bastos,
         Cart Rei Espases, Cart Dos Espases, Cart Tres Espases, Cart Manilla Espases,
         Cart Vuit Copes, Cart Cavall Copes, Cart Dos Copes, Cart As Copes,
         Cart Quatre Bastos, Cart Cinc Oros, Cart Vuit Oros, Cart Sis Bastos,
         Cart As Espases, Cart Set Espases, Cart Cavall Espases, Cart Vuit Espases,
         Cart Rei Copes, Cart Tres Oros, Cart Set Copes, Cart Manilla Copes,
         Cart Quatre Oros, Cart As Oros, Cart Sis Oros, Cart Rei Oros,
         Cart Cinc Bastos, Cart Sota Oros, Cart Tres Copes, Cart Rei Bastos,
         Cart Cinc Copes, Cart Sis Copes, Cart Cavall Oros, Cart Sota Copes,
         Cart Manilla Oros, Cart Quatre Espases, Cart Set Oros, Cart Sis Espases,
         Cart Sota Espases, Cart Set Bastos, Cart Quatre Copes, Cart Cinc Espases]

-- Analisis de l'exemple per  i assegurar-nos que tot funciona
jugada1= [Cart Manilla Bastos, Cart Sota Bastos, Cart As Bastos, Cart Dos Bastos] -- Guanya 1 Equip 1 (10) +1 per basa guanyada - (11,0)
jugada2=        [Cart Vuit Bastos, Cart Cavall Bastos, Cart Dos Oros, Cart Tres Bastos] -- Guanya 3 Equip 1 (2) +1 per basa guanyada - (14,0)
jugada3=        [Cart Rei Espases, Cart Dos Espases, Cart Tres Espases, Cart Manilla Espases] -- Guanya 4 Equip 2 (8) +1 per basa guanyada - (12,9)
jugada4=        [Cart Vuit Copes, Cart Cavall Copes, Cart Dos Copes, Cart As Copes] -- Guanya 4 Equip 1 (6) +1 per basa guanyada - (19,9)
jugada5=        [Cart Quatre Bastos, Cart Cinc Oros, Cart Vuit Oros, Cart Sis Bastos] -- Guanya 3 Equip 1 (0) +1 per basa guanyada - (20,9)
jugada6=        [Cart As Espases, Cart Set Espases, Cart Cavall Espases, Cart Vuit Espases] -- Guanya 1 Equip 1 (6) +1 per basa guanyada - (27,9)
jugada7=        [Cart Rei Copes, Cart Tres Oros, Cart Set Copes, Cart Manilla Copes] -- Guanya 2 Equip 2 (8) +1 per basa guanyada - (27,18)
jugada8=        [Cart Quatre Oros, Cart As Oros, Cart Sis Oros, Cart Rei Oros] -- Guanya 2 Equip 1 (7) +1 per basa guanyada - (35,18)
jugada9=        [Cart Cinc Bastos, Cart Sota Oros, Cart Tres Copes, Cart Rei Bastos] -- Guanya 2 Equip 2 (4) +1 per basa guanyada - (35,25)

jugada10= [Cart Cinc Copes, Cart Sis Copes, Cart Cavall Oros, Cart Sota Copes] -- Guanya 3 Equip 2 (3) +1 per basa guanyada - (35,29)
jugada11= [Cart Manilla Oros, Cart Quatre Espases, Cart Set Oros, Cart Sis Espases] -- Guanya 1 Equip 2 (5) +1 per basa guanyada - (35,35)
jugada12= [Cart Sota Espases, Cart Set Bastos, Cart Quatre Copes, Cart Cinc Espases] -- Guanya 1 Equip 2 (1) +1 per basa guanyada - (35,37)

-- Resultat d'executar el codi per saber el punts final de cada equip.
-- RESULTAT CORRECTE


main = do
    print (puntsParelles cartes1 (Trumf Oros) partida1 1)

--- >>> puntsParelles cartes1 (Trumf Oros) partida1 1
-- Just (37,35)

-- Per comparar cartes mirem que tant el TipusCarta com el Pal siguin iguals
instance Eq Carta  where
    Cart x z== Cart y s= x == y && z == s

-- Instancion el TipusCarta a Ord per poder ordenar-les, aixó fa que hagim de posar l'ordre de les 12 cartes
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

-- L'ordre de la carta depen unicament del número(TipusCarta)
instance Ord Carta where
    Cart x _<= Cart y _= x <= y
-- -- -- -- -- -- -- --
-- FUNCIONS PRINCIPALS
-- -- -- -- -- -- -- -- 

-- AUXILIAR DE TRAMPA. RETORNA LES JUGADES RESTANT
-- Entrant una llista de cartes, retornarà les bases restants, cada basa són 4 cartes
jugadesRestants :: [Carta] -> Int
jugadesRestants [] = 0
jugadesRestants (x:y:z:s:xs) = 1 + jugadesRestants xs
jugadesRestants _ = error "Error: s'ha perdut alguna carta"


-- AUXILIAR DE TRAMPA PER TREUE LES CARTES JA UTILITZADES
-- Un cop s'ha jugat una basa, eliminem les cartes jugades de les mans dels jugadors per evitar trampes de cartes repetides
cartesJugades :: [[Carta]] -> [Carta] -> [[Carta]]
cartesJugades [] _ = []
-- Si no queden cartes a treure ja hem acabat
cartesJugades x [] = x
-- S'elimina la carta de la mà de qui toqui i borrem la següent fins que no en quedin
cartesJugades x (y:ys) = cartesJugades (eliminaCarta x y) ys

eliminaCarta :: [[Carta]] -> Carta -> [[Carta]]
eliminaCarta [] _ = error "Error: intentant treure una carta inexistent"
eliminaCarta (x:xs) y
-- Si la primera mà té la carta, s'elimina, sinó, mirem la següent mà
 | conte x y = (elimina x y):xs
 | otherwise = x:(eliminaCarta xs y)

elimina :: [Carta] -> Carta -> [Carta]
elimina [] _ = []
elimina (x:xs) y
-- Si és la que busquem, retornem totes menys aquesta
 | x == y = xs
-- Si no és la que busquem, retornem la carta actual més la resta de la llista eliminant la carta entrada
 | otherwise = x:(elimina xs y)

-- COMPROVAR SI HI HA HAGUT ALGUNA TRAMPA DURANT LA PARTIDA
trampa :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
-- No hi ha trampes perque no hi ha cartes a les mans del jugadors
trampa [] _ _ _ = Nothing
-- No hi ha trampes perque no queden cartes que s'hagin jugat
trampa _ _ [] _ = Nothing
trampa x y (c:v:b:n:zs) s
 --  Si no hi ha trampes en aquesta basa, busquem a la següent, teient de les mans dels jugadors les cartes utilitzades
 | isNothing(basaCorrecta x y [c,v,b,n] s) = trampa (cartesJugades x [c,v,b,n]) y zs (quiSortira s (snd (quiGuanya [c,v,b,n] y)))
-- La basa actual, les 12 jugades totals menys les que queden (cada bada 4 cartes) i com que sabem que hi ha hagut trampes, tenim un Just, i extreiem les dades
 | otherwise = Just ([c,v,b,n],12-(jugadesRestants zs),fromJust(basaCorrecta x y [c,v,b,n] s))


-- CONTAR ELS PUNTS D'UNA LLISTA DE CARTES
punts :: [Carta] -> Int
punts [] = 0
punts ((Cart Manilla _):xs) = 5 + punts xs
punts ((Cart As _):xs) = 4 + punts xs
punts ((Cart Rei _):xs) = 3 + punts xs
punts ((Cart Cavall _):xs) = 2 + punts xs
punts ((Cart Sota _):xs) = 1 + punts xs
punts (_:xs) = punts xs


-- Retorna la llista de cartes guanyades depenent de si ets equip parell o imparell
-- Hi ha dos equips, els parells i els imparells. Qui començà tirant és l'equip imparell. A cada jugada pot ser que es canvii l'ordre.
-- Si ha guanyat un jugador imparell, guanya l'equip imparell, altrament, si les l'equip imparell, es canvia al parell i es miren les següents jugades.
-- Si ets l'equip parell i guanyen els parells, aquest equip passa a ser el parell a la següent jugada.
cartesEquip :: Trumfu -> [Carta] -> Int -> [Carta]
cartesEquip _ [] _ = []
cartesEquip x (c:v:b:n:y) m
-- Sequim siguent els 1, per tant, seguim siguent imparell
 | odd m && odd (snd(quiGuanya [c,v,b,n] x)) = c:v:b:n:(cartesEquip x y m)
 | odd m = (cartesEquip x y (m+1))
 -- El primer és imparell, per tant, ara passes a ser imparells
 | even m && even (snd(quiGuanya [c,v,b,n] x)) = c:v:b:n:(cartesEquip x y (m+1))
 | otherwise = (cartesEquip x y m)


-- RETORNA UNA TUPLA AMB LES CARTES GUANYADES PER CADA EQUIP
cartesGuanyades:: Trumfu -> [Carta] -> Int -> ([Carta],[Carta])
cartesGuanyades _ [] _ = ([],[])
cartesGuanyades x y z = ((cartesEquip x y 1),(cartesEquip x y 2))


puntsParelles:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)
puntsParelles [] _ _ _ = Nothing
puntsParelles _ _ [] _ = Nothing
puntsParelles x y z s
-- Si no hi ha trampa es retorna els punts de les cartes guanyades més el de les bases (+1 punt per basa guanyada)
 | isNothing(trampa x y z s) = puntsAmbBases (cartesGuanyades y z s)
 | otherwise = Nothing

-- Hi entra les cartes guanyades per cada equip i retorna una tupla amb els punts de cada llista més els punts per guanyar la basa
puntsAmbBases :: ([Carta], [Carta]) -> Maybe (Int,Int)
--Per cada equip hem de sumar els punts de les cartes i el de les bases guanyades
puntsAmbBases x = Just (punts (fst x)+bases (fst x),punts (snd x)+bases (snd x))

--Contem les bases que hi ha a la llista de cartes
bases :: [Carta] -> Int
bases [] = 0
bases (x:c:v:b:xs) = 1+bases xs
bases _ = error "Error: s'ha perdut alguna carta"


-- -- -- -- -- -- -- --
-- FUNCIONS AUXILIARS
-- -- -- -- -- -- -- -- 

-- RETORNA LLISTA DE CARTES QUE CONCORDEN AMB EL PAL
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal [] _ = []
cartesPal ((Cart x y):xs) z
-- Si la carta té el pal entrat, es retorna aquesta més les altres que hi pugui haver, altrament retornem les altres que hi pugui haver
 | y == z = (Cart x y):(cartesPal xs z)
 | otherwise = (cartesPal xs z)


-- RETORNA EL PAL GUANYADOR DE LA LLISTA DE CARTES
-- El pal guanyador és el pal de la primera carta, a no ser que hi aparegui en algun moment el pal trumfu
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa ((Cart x z):xs) (Trumf y)
 | (cartesPal ((Cart x z):xs) y) == [] = z
 | otherwise = y

-- RETORNA LA CARTA MES ALTA DE LA LLISTA
-- Busquem la carta més altra que hi hagi a la llista
mesAlt :: [Carta] -> Carta
mesAlt [] = error "Error: llista buida"
mesAlt [x] = x
mesAlt (x:xs)
 | x > (mesAlt xs) = x
 | otherwise = mesAlt xs


-- RETORNA LA POSICIO DE LA CARTA A LA LLISTA
-- Retornem la posició, dins la llista, de la carta entrada
posCarta :: [Carta] -> Carta -> Int
posCarta (x:xs) y
 | x /= y = 1 + (posCarta xs y)
 | otherwise = 1

-- RETORNA LA POSICIO DE LA CARTA GUANYADORA
-- El guanyador és la carta més alta dins la basa i la posició d'aquesta és el jugador que l'ha tirat.
-- Per tant, hem de buscar la carta més alta de la llista de cartes del pal guanyador.
quiGuanya :: [Carta] -> Trumfu -> (Carta,Int)
quiGuanya x (Trumf y)= ((mesAlt (cartesPal x (palGuanyadorBasa x (Trumf y)))),(posCarta x (mesAlt (cartesPal x (palGuanyadorBasa x (Trumf y))))))


-- RETORNA LA PERSONA QUE COMENÇARA LA SEGÜENT BASA
-- Donat qui començat i la posició de qui ha guanyat, retornem el jugador que comença la següent basa
quiSortira :: Int -> Int -> Int
quiSortira x y
 | x + y - 1 <= 4 = x + y - 1
 | otherwise = x + y - 5
-- el -1 és perque el primer a tirar es el mateix que tira, per tant si guanya el 2 nomes ha de moure 1, -5 és (-4) + (-1)

-- RETORNA LA LLISTA DE CARTES QUE ES PODEN JUGAR
-- Les cartes que es poden jugar són les del pal que s'ha tirat més les del trumfu, per això fem una concanatació de les dues llistes.
-- En cas de ser el primer a tirar, pots tirar qualsevol carta que tinguis
jugades :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugades x _ [] = x
jugades x (Trumf y) ((Cart z s):zs)
 | (cartesPal x s) == [] = x
 | otherwise = (cartesPal x s)++(cartesPal x y)


-- RETORNA SI LA LLISTA CONTE LA CARTA O NO
-- Retorna true si la carta entrada està a la llista, false altrament
conte :: [Carta] -> Carta -> Bool
conte [] _ = False
conte (x:xs) y
 | x==y = True
 | otherwise = conte xs y

-- Retorna la mà del jugador entrat
-- La posició del jugador serà sempre la 1, ja que si no ho és, retallem la llista i tornem a mirar-ho
cartesJugador :: [[Carta]] -> Int -> [Carta]
cartesJugador [] _ = error "Error: llista buida"
cartesJugador (x:xs) y
 | y<1 || y>4 = error "Error: no existeix aquest usuari"
 | y == 1 = x
 | otherwise = cartesJugador xs (y-1)

-- Retorna si a la basa hi ha algut alguna trampa.
-- Si retorna Nothing, no hi ha hagut trampa
-- Si retorna Just amb un enter, retorna la posició on hi ha hagut la trampa
basaCorrecta :: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe(Int)
basaCorrecta [] _ _ _ = Nothing
basaCorrecta _ _ [] _  = Nothing
basaCorrecta x y (s:sx) z
-- Si ja ha jugat el jugador 4, ara l'hi toca al primer no al cinquè, per tant, restem 4
 | z > 4 = basaCorrecta x y (s:sx) (z-4)
 --Si la carta tirada està a la llista del que pot tirar no hi ha hagut trampa i mirem el següent jugador
 | (conte (jugades (cartesJugador x z) y (s:sx)) s) = basaCorrecta x y sx (z+1)
-- Si arrivem aqui hi ha hagut trampa, per tant, retornem la jugada en la que ha passat (dins la basa)
 | otherwise = Just z

