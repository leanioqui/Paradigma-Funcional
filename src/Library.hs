module Library where
import PdePreludat
import Data.Kind (FUN)
import GHC.Num (Num)
import qualified Test.Hspec as Evaluation

data Videojuego = Videojuego {
  titulo      :: String,
  anio        :: Number,
  expansiones :: [String],
  precio      :: Number
} deriving (Show, Eq)


-- Juegos
zelda :: Videojuego
zelda = Videojuego "The Legend of Zelda" 1986 ["Breath","Tears","Echoes","Awakening"] 3000

pacman :: Videojuego
pacman = Videojuego "Pac-Man" 1980 ["Championship"] 200

hollowKnight :: Videojuego
hollowKnight = Videojuego "Hollow Knight" 2017 [] 500

cyberpunk :: Videojuego
cyberpunk = Videojuego "Cyberpunk 2077" 2020 ["Phantom Liberty"] 4000

hades :: Videojuego
hades = Videojuego "Hades" 2020 [] 1000

witcher3 :: Videojuego
witcher3 = Videojuego "The Witcher 3" 2015 ["Hearts of Stone"] 2000

skyrim :: Videojuego
skyrim = Videojuego "Skyrim" 2011 ["Dragonborn","Dawnguard"] 1000

ageOfEmpires :: Videojuego
ageOfEmpires = Videojuego "Age of Empires II" 1999 [] 500

alanWake :: Videojuego
alanWake = Videojuego "Alan Wake" 2010 ["The Writer","The Director Edition"] 800

deathStranding :: Videojuego
deathStranding = Videojuego "Death Stranding" 2019 ["director cut","Directors"] 3000

control :: Videojuego
control = Videojuego "Control" 2019 ["Foundation","AWE"] 2000

rimworld :: Videojuego
rimworld = Videojuego "Rimworld" 2018 ["Ideology","Biotech","Royalty"] 1500

stellaris :: Videojuego
stellaris = Videojuego "Stellaris" 2016 ["Utopia","Megacorp","Federations"] 2000

-- Auxiliar
cantExpansiones :: Videojuego -> Number
cantExpansiones = length . expansiones

-- Impacto
impacto :: Videojuego -> Number
impacto juego
  | anio juego < 1990 && cantExpansiones juego > 3 = precio juego * cantExpansiones juego
  | anio juego < 1990                              = precio juego + 200 * (1990 - anio juego)
  | cantExpansiones juego == 0                     = precio juego / 2
  | otherwise                                      = precio juego + 50 * cantExpansiones juego

-- Expansión de Culto Punto 2 Integrante 1 
expansionDeCulto :: Videojuego -> Bool
expansionDeCulto juego = any (elem "Director" . words) (expansiones juego)

-- Es Nicho Punto 2 Integrante 2
esNicho :: Number -> Videojuego -> Bool
esNicho cantMaxPermitida = ( cantMaxPermitida >= ) . length . filter ( ( 10 < ) . length ) . expansiones

-- Titulo denso Punto 2 Integrante 3
esDenso :: Number -> Videojuego -> Bool
esDenso cantElegidaLetras = all (> cantElegidaLetras) . map length . words . titulo

-- Eventos
lanzarExpansion :: String -> Videojuego -> Videojuego
lanzarExpansion nuevaExpansion juego = juego {
  expansiones = expansiones juego ++ [nuevaExpansion],
  precio = precio juego + 100 + 30 * cantExpansiones juego
  }

-- La función recibe "cómo cambia el precio" y el "juego"
modificarPrecio :: (Number -> Number) -> Videojuego -> Videojuego
modificarPrecio transformacion juego = juego { 
    precio = transformacion (precio juego) 
}

-- Parchear Punto 3 Integrante 1 
parchear :: Number -> Videojuego -> Videojuego
parchear largoMinimo juego = 
    modificarPrecio (+ 100 * cantExpansiones juegoFiltrado) juegoFiltrado
    where 
        -- Paso 1: Filtramos las expansiones una sola vez 
        nuevasExp = filter ((>= largoMinimo) . length) (expansiones juego)
        -- Paso 2: Creamos una versión del juego con la lista actualizada
        juegoFiltrado = juego { expansiones = nuevasExp }

-- Remasterizar Punto 3 Integrante 2
remasterizar :: Videojuego -> Videojuego
remasterizar juego = juego {
  precio = ((precio juego +) . (100 *) . length . titulo) juego,
  titulo = titulo juego ++ " Remastered"
  }

-- Relanzar Punto 3 Integrante 3
relanzar :: Videojuego -> Videojuego
relanzar juego = juego {
  precio = precio juego * 2,
  titulo = titulo juego ++ " Legacy",
  expansiones = expansiones juego ++ ["Director's Cut"]
  }

-- Punto 4
laCruelVidaDelVideojuego :: String -> Number -> Videojuego -> Videojuego
laCruelVidaDelVideojuego nuevaExpansion largoMinimo = relanzar. remasterizar. parchear largoMinimo. lanzarExpansion nuevaExpansion

-- laCruelVidaDelVideojuego "Revenge" 5 hades

-- 

--- Entrega 2

--Punto 1 — Una temporada para recordar

-- Punto 1.1 - Las temporadas pasan... (todos los integrantes)

data Temporada = Temporada {
  numero :: Number,
  listaDeEventos :: [Videojuego -> Videojuego]
}deriving (Show)


-- Definición de las temporadas según la consigna para los tests
temporada2021 = Temporada 2021 [lanzarExpansion "Parche", parchear 6]
temporada2022 = Temporada 2022 [parchear 8, remasterizar, relanzar]
temporada2023 = Temporada 2023 [lanzarExpansion "DLC1", parchear 5, remasterizar, relanzar]
temporadaVacia = Temporada 2020 []

--Eq no se puede poner dado que no se pueden comparar Funciones, tira error

pasarTemporada ::  Temporada -> Videojuego -> Videojuego
pasarTemporada temporada juego = foldl aplicarEvento juego (listaDeEventos temporada)

aplicarEvento :: Videojuego -> (Videojuego -> Videojuego) -> Videojuego
aplicarEvento juego evento = evento(juego)

type Criterio = Videojuego -> Number

mejoroSegun :: Criterio -> (Videojuego -> Videojuego) -> Videojuego -> Bool
mejoroSegun miCriterio evento juego = miCriterio juego < (miCriterio.evento) juego

-----------------------------Reutilizar ideas comunes entre 1.2, 1.3 y 1.4 sin copy-paste.-----------------------------------

empeoroSegun :: Criterio -> (Videojuego -> Videojuego) -> Videojuego -> Bool
empeoroSegun miCriterio evento juego = miCriterio juego > (miCriterio.evento) juego

type Condicion =  Videojuego -> (Videojuego -> Videojuego) -> Bool

soloEventosQue :: Condicion -> Videojuego -> Temporada -> [Videojuego -> Videojuego]
soloEventosQue condicion juego temporada = filter (condicion juego) (listaDeEventos temporada)

--Punto 1.2

mejoranElPrecio:: Condicion
mejoranElPrecio juego evento = mejoroSegun precio evento juego 


--Punto 1.3

achicanElCatalogo:: Condicion
achicanElCatalogo juego evento = empeoroSegun cantExpansiones evento juego

mejoranElCatalogo:: Condicion
mejoranElCatalogo juego evento = mejoroSegun cantExpansiones evento juego

--Punto 1.4

mejoranElImpacto :: Condicion
mejoranElImpacto juego evento = mejoroSegun impacto evento juego

--- Abstraccion Final

aplicarSoloEventosQue :: Condicion -> Temporada -> Videojuego -> Videojuego
aplicarSoloEventosQue condicion temporada juego = foldl aplicarEvento juego (soloEventosQue condicion juego temporada)

-- Punto 1.2 final (Integrante 1)
aplicarEventosQueMejoranPrecio :: Temporada -> Videojuego -> Videojuego
aplicarEventosQueMejoranPrecio = aplicarSoloEventosQue mejoranElPrecio

-- Punto 1.3 final (Integrante 2)
aplicarEventosQueAchicanCatalogo :: Temporada -> Videojuego -> Videojuego
aplicarEventosQueAchicanCatalogo = aplicarSoloEventosQue achicanElCatalogo

-- Punto 1.4 final (Integrante 3)
aplicarEventosQueMejoranImpacto :: Temporada -> Videojuego -> Videojuego
aplicarEventosQueMejoranImpacto = aplicarSoloEventosQue mejoranElImpacto

-- Punto 2: Analiticssssssss

--Punto 2.1 (Integrante 1)

-- La consigna aclara que debe haber al menos un evento, sino se deberia de aplicar "Maybe"

preciosAscendentes :: [Videojuego -> Videojuego] -> Videojuego -> Bool
preciosAscendentes [] juego = True
preciosAscendentes (evento:eventos) juego | mejoroSegun precio evento juego = preciosAscendentes eventos juego 
                                          | otherwise = False

--Punto 2.2 (Integrante 2) 

juegosOrdenadosPor :: Criterio -> [Videojuego] -> Bool
juegosOrdenadosPor miCriterio [juego1] = False

juegosOrdenadosPor miCriterio [juego1, juego2] | miCriterio juego1 < miCriterio juego2 = True
                                                | otherwise = False
juegosOrdenadosPor miCriterio (juego1:juego2:juegos) | miCriterio juego1 < miCriterio juego2 = juegosOrdenadosPor miCriterio (juego2:juegos)
                                                      |otherwise = False

--Punto 2.3 (Integrante 3) 

impactosAscendentes :: [Temporada] -> Videojuego -> Bool
impactosAscendentes [temporada1] juego = impacto juego < (impacto.pasarTemporada temporada1) juego  

impactosAscendentes [temporada1,temporada2] juego | (impacto.pasarTemporada temporada1) juego  < (impacto.pasarTemporada temporada2) (pasarTemporada temporada1 juego) = True
                                                  | otherwise = False
impactosAscendentes (temporada1:temporada2:restoTemporadas) juego | (impacto.pasarTemporada temporada1) juego  < (impacto.pasarTemporada temporada2) (pasarTemporada temporada1 juego) = impactosAscendentes (temporada2:restoTemporadas) juego
                                                                  | otherwise = False

--Punto 3.1 Precios ascendentes 
temporada2024 :: Temporada
temporada2024 = Temporada { numero = 2024, listaDeEventos = [lanzarExpansion "DLC0", relanzar ] ++ map parchear [1..] }

{-
Si y gracias a la lazy evaluation, si bien lanzarExpansion "DLC0" y relanzar van a aumentar el precio, va a haber algun parchear x que supere la cantidad de los caracteres de las expansiones
 finalmente dara false
-}


-- Punto 3.2 - Juegos Ordenados (Tu versión optimizada)
alternarJuegos :: Videojuego -> Videojuego -> Number -> Videojuego
alternarJuegos juego1 juego2 numero
  | odd numero  = juego1
  | even numero   = juego2

juegosInfinitos :: [Videojuego]
juegosInfinitos = [zelda, hollowKnight] ++ map (alternarJuegos witcher3 cyberpunk) [1..]

{-
Si, y sera False dado a que el criterio que nosotros evaluemos en algun punto de la infinidad de la lista sera menor al anterior, 
esto es posible a la Lazy Evaluation o Evaluacion diferida de Haskell
-}

--Punto 3.3 Impactos ascendentes

{-
Si la lista de temporadas es infinita pero repetitiva si, dado a que va a llegar un momento donde el impacto de una no sea menor a la de su siguiente, dado a la Lazy Evaluation.
-}


-------------------------------------------------------------------------------------------------------------------------------------------------------

--PRACTICA DE EVALUACION

temporadasExigentes :: Criterio -> Temporada -> Videojuego-> Bool
temporadasExigentes miCriterio temporada juego = all (\evento -> mejoroSegun miCriterio evento juego) (listaDeEventos temporada)

filtrosAcumulativos :: [Videojuego -> Videojuego] -> Videojuego -> Number -> Videojuego
filtrosAcumulativos [] juego limite = juego
filtrosAcumulativos (evento1:restoDeEventos) juego limite | precio juego < limite = filtrosAcumulativos restoDeEventos (aplicarEvento juego evento1) limite
                                                          | otherwise = filtrosAcumulativos restoDeEventos juego limite

expansionesInfinitas :: String -> [String]
expansionesInfinitas = (\prefijo -> map ((prefijo ++  " V")++) (map show [1..]))