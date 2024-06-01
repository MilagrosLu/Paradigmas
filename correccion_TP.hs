import Prelude
import Text.Show.Functions
import Data.List

type Atraccion = String
data Ciudad = Ciudad {
    nombre:: String,
    anioFundacion :: Int,
    atracciones :: [Atraccion],
    costoDeVida :: Float
} deriving (Show, Ord, Eq)



-------------------------------------------------------------------------------------------------------------------

-- Declaraciones Ciudades

baradero :: Ciudad
baradero = Ciudad {
    nombre = "Baradero",
    anioFundacion = 1615,
    atracciones = ["Parque del Este","Museo Alejandro Barbich"],
    costoDeVida = 150
}

nullish :: Ciudad
nullish = Ciudad {
    nombre = "Nullish",
    anioFundacion = 1800,
    atracciones = [],
    costoDeVida = 140
}

caletaOlivia :: Ciudad
caletaOlivia = Ciudad {
    nombre = "Caleta Olivia",
    anioFundacion = 1901,
    atracciones = ["El Gorosito", "Faro Costanera"],
    costoDeVida = 120
}

azul :: Ciudad
azul = Ciudad {
    nombre = "Azul",
    anioFundacion = 1832,
    atracciones = ["Teatro Espanol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"],
    costoDeVida = 190
}

maipu :: Ciudad
maipu = Ciudad {
    nombre = "Maipu",
    anioFundacion = 1878,
    atracciones = ["Fortin Kakel"],
    costoDeVida = 115
}

-- Declaraciones Años

anio2021 :: Anio
anio2021 =  [crisis, sumarAtraccion "playa"]

anio2022 :: Anio
anio2022 =[crisis, remodelacion 5, reevaluacion 7]

anio2023 :: Anio
anio2023 =  [crisis, sumarAtraccion "parque", remodelacion 10, remodelacion 20]

anio2015 :: Anio
anio2015 =  [] 

-- Declaraciones de orden + eventos

listaCiudadesOrdenadas1 :: [Ciudad]
listaCiudadesOrdenadas1 = [caletaOlivia, nullish, baradero, azul]

listaCiudadesOrdenadas2 :: [Ciudad]
listaCiudadesOrdenadas2 = [caletaOlivia, azul, baradero]

remodelacion10 :: Evento
remodelacion10 = remodelacion 10

-------------------------------------------------------------------------------------------------------------------

-- Punto 1: Valor de una ciudad --

valorCiudad :: Ciudad -> Float
valorCiudad (Ciudad _ unAnioFundacion unasAtracciones unCostoDeVida)
    | unAnioFundacion < 1800 = 5 * (1800 - fromIntegral unAnioFundacion)
    | null unasAtracciones = 2 * unCostoDeVida
    | otherwise =  3 * unCostoDeVida

-------------------------------------------------------------------------------------------------------------------

-- Punto 2: Características de las ciudades -- 
-- a) Atraccion Copada

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada = any (esVocal . head) . atracciones

----------------------------------------------------------
--b) Ciudad Sobria 

esCiudadSobria :: Int -> Ciudad -> Bool
esCiudadSobria cantidadLetras = all ((> cantidadLetras) . length) . atracciones 


----------------------------------------------------------

--c) Ciudad Con Nombre Raro

esCiudadConNombreRaro :: Ciudad -> Bool
esCiudadConNombreRaro = (<5) . length . nombre 

-------------------------------------------------------------------------------------------------------------------

-- Punto 3: Eventos -- 


modificarCostoDeVida :: (Float -> Float) -> Ciudad -> Ciudad
modificarCostoDeVida modificador unaCiudad= unaCiudad {costoDeVida = modificador . costoDeVida $ unaCiudad}

modificarAtracciones :: ([Atraccion] -> [Atraccion]) -> Ciudad -> Ciudad
modificarAtracciones modificador unaCiudad = unaCiudad {atracciones = modificador . atracciones $ unaCiudad}

modificarNombre :: (String -> String) -> Ciudad -> Ciudad
modificarNombre modificador unaCiudad = unaCiudad {nombre = modificador . nombre $ unaCiudad}

type Evento = Ciudad -> Ciudad

-- a) Sumar Atraccion
sumarAtraccion :: Atraccion -> Evento
sumarAtraccion nuevaAtraccion = modificarCostoDeVida (*1.2) . modificarAtracciones (nuevaAtraccion :) 
-- b) Crisis
crisis :: Evento
crisis = modificarCostoDeVida (*0.9) . modificarAtracciones (\atracciones -> take (length atracciones - 1) atracciones) 
-- c) Remodelacion

calculoPorcentaje :: Float -> Float -> Float 
calculoPorcentaje porcentaje = ((100 + porcentaje) / 100 *)

remodelacion :: Float -> Evento 
remodelacion porcentaje = modificarNombre ("New " ++) . modificarCostoDeVida (calculoPorcentaje porcentaje)
-- d) Reevaluacion
reevaluacion :: Int -> Evento
reevaluacion cantidadLetras ciudadInicial
    | esCiudadSobria cantidadLetras ciudadInicial = modificarCostoDeVida (*1.1) ciudadInicial
    | otherwise = modificarCostoDeVida (subtract 3) ciudadInicial

-------------------------------------------------------------------------------------------------------------------

-- Punto 4: La transformación no para --

-- AZUL CON NUEVA ATRACCION
-- sumarAtraccion "Balneario Municipal Alte. Guillermo Brown" azul
-- Ciudad {nombre = "Azul", anioFundacion = 1832, atracciones = ["Balneario Municipal Alte. Guillermo Brown","Teatro Espanol","Parque Municipal Sarmiento","Costanera Cacique Catriel"], costoDeVida = 228.00002} 

--AZUL CON CRISIS
-- crisis azul 
--Ciudad {nombre = "Azul", anioFundacion = 1832, atracciones = ["Teatro Espanol","Parque Municipal Sarmiento"], costoDeVida = 171.0}

--NULLISH CON CRISIS
-- crisis nullish 
-- Ciudad {nombre = "Nullish", anioFundacion = 1800, atracciones = [], costoDeVida = 126.0}

--AZUL CON REMODELACION 
--remodelacion 50 azul 
--Ciudad {nombre = "New Azul", anioFundacion = 1832, atracciones = ["Teatro Espanol","Parque Municipal Sarmiento","Costanera Cacique Catriel"], costoDeVida = 285.0}

--AZUL CON REEVALUACION 
--ghci> reevaluacion 14 azul 
--Ciudad {nombre = "Azul", anioFundacion = 1832, atracciones = ["Teatro Espanol","Parque Municipal Sarmiento","Costanera Cacique Catriel"], costoDeVida = 187.0}

--AZUL REEVALUACION 13 
--ghci> reevaluacion 13 azul 
--Ciudad {nombre = "Azul", anioFundacion = 1832, atracciones = ["Teatro Espanol","Parque Municipal Sarmiento","Costanera Cacique Catriel"], costoDeVida = 209.0}


-------------------------------------------------------------------------------------------------------------------
-- Punto 5 : Un año para recordar -- 
--Suponemos que los eventos son escritos en la lista de manera cronológica 


type Anio = [Evento]

reflejarPasoAnio :: Anio -> Ciudad -> Ciudad
reflejarPasoAnio = flip (foldr ($))

-- Generar Criterios
mejoroCiudadEn :: Ord a => (Ciudad -> a) -> Ciudad -> Evento -> Bool
mejoroCiudadEn unCriterio ciudadInicial evento = unCriterio ciudadInicial < unCriterio ciudadAfectada
    where 
        ciudadAfectada = evento ciudadInicial

aplicarEventosSegun :: (Evento -> Bool) -> Anio -> Ciudad -> Ciudad
aplicarEventosSegun criterio anio = reflejarPasoAnio (filter criterio anio) 

costoVidaBaje :: Anio-> Ciudad -> Ciudad
costoVidaBaje anio unaCiudad = aplicarEventosSegun (not . mejoroCiudadEn costoDeVida unaCiudad) anio unaCiudad

costoDeVidaSuba :: Anio-> Ciudad -> Ciudad
costoDeVidaSuba anio unaCiudad = aplicarEventosSegun (mejoroCiudadEn costoDeVida unaCiudad) anio unaCiudad

valorCiudadSuba :: Anio-> Ciudad -> Ciudad
valorCiudadSuba anio unaCiudad = aplicarEventosSegun (mejoroCiudadEn valorCiudad unaCiudad) anio unaCiudad
-------------------------------------------------------------------------------------------------------------------

-- Punto 6: Funciones a la orden --
estanOrdenados :: Ord a => [a] -> Bool
estanOrdenados [] = False
estanOrdenados [x] = True 
estanOrdenados (x: y : xs) = x <= y && estanOrdenados (y:xs)
-- 6.1) Eventos ordenados
costoDeVidaAfectado :: Ciudad -> Evento -> Float
costoDeVidaAfectado unaCiudad evento = costoDeVida . evento $ unaCiudad

mapEstanOrdenados :: Ord a1 => (a2 -> a1) -> [a2] -> Bool
mapEstanOrdenados f = estanOrdenados . map f

eventosOrdenados :: Ciudad -> Anio -> Bool
eventosOrdenados unaCiudad = mapEstanOrdenados (costoDeVidaAfectado unaCiudad)

-- 6.2) Ciudades ordenadas
ciudadesOrdenadas :: Evento -> [Ciudad] -> Bool
ciudadesOrdenadas evento = mapEstanOrdenados (flip costoDeVidaAfectado evento)

-- 6.3) Años ordenados
serieCostosDeVidaAscendente :: Ciudad -> [Anio] ->  Bool
serieCostosDeVidaAscendente  ciudad =  mapEstanOrdenados (costoDeVida . flip reflejarPasoAnio ciudad)
-------------------------------------------------------------------------------------------------------------------

-- Parte 7:  Al infinito, y más allá... --

--Eventos Ordenados

dosMilVeinticuatro :: Anio
dosMilVeinticuatro =  crisis : reevaluacion 7 : map remodelacion [1..]

{-
De la manera en la que esta definida eventosOrdenados, al usar Lazy Evaluation, va analizando evento por evento.
Por lo tanto, si uno ya da False devolverá eso. En cambio, si ninguno de los eventos baja el costo de vida, 
se quedará "eternamente" analizando la lista infinita. 
-}

-- Ciudades ordenadas
discoRayado :: [Ciudad]
discoRayado = azul : nullish : cycle [caletaOlivia, baradero]

{-
Gracias a la evaluación diferida es posible obtener un resultado para ciudadesOrdenadas y discoRayado
siempre y cuando se encuentre alguna ciudad para la cual el evento no mejore su costo de vida. Al evaluar ciudad por ciudad,
que una de False ya hará que la funcion all devuleva eso. 
Si todas las ciudades mejoran su costo de vida con el evento la funcion se quedara evaluando "eternamente" (hasta que el proceso sea matado)
la lista. 
-}

-- Años ordenados
laHistoriaSinFin :: [Anio]
laHistoriaSinFin = anio2021 : anio2022 : repeat anio2023

{-
Por la manera que esta definida y gracias al Lazy Evaluation, se va tomando año por año y comparando. 
Como solo tener un False ya hace False al resultado, éste es un resultado posible. 
Sin embargo, de no haber un False se colgará analizando la lista infinita de años. 
-}