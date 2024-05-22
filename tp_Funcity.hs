-- *** Dejo lo del genericLength por si lo llegamos a usar ***
--import Data.List(genericLength)
-- * genericLength :: Num i => [a] -> i
-- -- Esta función es exactamente igual que length,
-- -- con la única diferencia que no devuelve un Int, sino un número
-- -- fácil de operar con otro número que pueden o no ser enteros.
-- --
-- -- -- ghci> length "Luigi Mario" / 2
-- -- -- error:
-- -- --     • No instance for (Fractional Int) arising from a use of ‘/’
-- -- --     • In the expression: length "Luigi Mario" / 2
-- -- --       In an equation for ‘it’: it = length "Luigi Mario" / 2
-- -- -- ghci> genericLength "Luigi Mario" / 2
-- -- -- 5.5

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
import Prelude
import Text.Show.Functions
import Data.List
import GHC.Exts.Heap (GenClosure(what_next))
import Data.Functor.Classes (Ord2)
data Ciudad = Ciudad {
    nombre:: String,
    anioFundacion :: Int,
    atracciones :: [String],
    costoDeVida :: Float
} deriving (Show, Ord, Eq)
data Evento =
    SumarAtraccion String
    | Crisis
    | Remodelacion Float
    | Reevaluacion Int
    deriving (Show)

data AnioParaRecordar = AnioParaRecordar{
    anio :: Int,
    eventos :: [Evento]
} deriving (Show)

data Criterio = CostoDeVida | CantidadDeAtracciones | ValorCiudad


-------------------------------------------------------------------------------------------------------------------

-- DECLARACIONES PARA TESTEAR --  // Eliminar antes de entregar  
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

anio2022 = AnioParaRecordar {
    anio = 2022,
    eventos = [Crisis, Remodelacion 5, Reevaluacion 7]
}
anio2023 = AnioParaRecordar {
    anio = 2023,
    eventos = [Crisis, SumarAtraccion "parque" , Remodelacion 10, Remodelacion 20]
}

anio2021 = AnioParaRecordar {
    anio = 2021,
    eventos= [Crisis, SumarAtraccion "playa"]
}
-------------------------------------------------------------------------------------------------------------------

-- Punto 1: Valor de una ciudad --

valorCiudad ::Ciudad -> Float
valorCiudad (Ciudad _ unAnioFundacion unasAtracciones unCostoDeVida)
    | unAnioFundacion < 1800 = 5 * 1800 - fromIntegral unAnioFundacion
    | null unasAtracciones = 2 * unCostoDeVida
    | otherwise =  3 * unCostoDeVida



-------------------------------------------------------------------------------------------------------------------

-- Punto 2: Características de las ciudades -- 
-- a) Atraccion Copada

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

atraccionCopada :: Ciudad -> Bool
atraccionCopada = any esAtraccionCopada . atracciones
    where
        esAtraccionCopada = esVocal . head 

----------------------------------------------------------

--b) Ciudad Sobria 

ciudadSobria :: Int -> Ciudad -> Bool
ciudadSobria cantidadLetras = all (tieneMasDeXLetras cantidadLetras ) . atracciones 
    where
        tieneMasDeXLetras cantidadLetras  unaAtraccion =  length unaAtraccion > cantidadLetras

----------------------------------------------------------

--c) Ciudad Con Nombre Raro

ciudadNombreRaro :: Ciudad -> Bool
ciudadNombreRaro (Ciudad unNombre _ _ _) = length unNombre  < 5

-------------------------------------------------------------------------------------------------------------------

-- Punto 3: Eventos -- 

aplicarEvento :: Ciudad -> Evento -> Ciudad

-- a) Sumar Atraccion
aplicarEvento ciudadInicial (SumarAtraccion nuevaAtraccion) = ciudadInicial { atracciones = nuevaAtraccion : atracciones ciudadInicial, costoDeVida = 1.2 * costoDeVida ciudadInicial }

-- b) Crisis
aplicarEvento ciudadInicial Crisis
    | null . atracciones $ ciudadInicial= ciudadInicial { costoDeVida =  costoDeVidaNuevo , atracciones = [] }
    | otherwise = ciudadInicial { costoDeVida = costoDeVidaNuevo  , atracciones = init . atracciones $ ciudadInicial }
    where 
        costoDeVidaNuevo = (* 0.9) . costoDeVida $ ciudadInicial
    
-- c) Remodelacion
aplicarEvento ciudadInicial (Remodelacion porcentaje) = ciudadInicial { nombre = "New " ++ nombre ciudadInicial, costoDeVida = ((100 + porcentaje) / 100) * costoDeVida ciudadInicial}

-- d) Reevaluacion
aplicarEvento ciudadInicial (Reevaluacion cantLetras)
    | cumpleCondicion = ciudadInicial { costoDeVida = 1.1 * costoDeVida ciudadInicial }
    | otherwise = ciudadInicial { costoDeVida =  max 0 (costoDeVida ciudadInicial - 3) }
    where
        cumpleCondicion = ciudadSobria cantLetras ciudadInicial

-------------------------------------------------------------------------------------

-- Punto 4: La transformación no para --

azulConNuevaAtraccion :: Ciudad
azulConNuevaAtraccion = aplicarEvento azul (SumarAtraccion "Balneario Municipal Alte. Guillermo Brown")

azulConCrisis :: Ciudad
azulConCrisis = aplicarEvento azul Crisis

nullishConCrisis :: Ciudad
nullishConCrisis = aplicarEvento nullish Crisis

azulConRemodelacion :: Ciudad
azulConRemodelacion = aplicarEvento azul (Remodelacion 50)

azulConReevaluacion :: Ciudad
azulConReevaluacion = aplicarEvento azul (Reevaluacion 14)

azulConReevaluacion2 :: Ciudad
azulConReevaluacion2 = aplicarEvento azul (Reevaluacion 13)

-------------------------------------------------------------------------------------
--Punto 5 : Un año para recordar
--Suponemos que los eventos son escritos en la lista de manera cronológica 

reflejarPasoAnio :: AnioParaRecordar -> Ciudad -> Ciudad
reflejarPasoAnio (AnioParaRecordar anio eventos) unaCiudad
    | null eventos = unaCiudad
    | otherwise = reflejarPasoAnio (AnioParaRecordar anio (tail eventos)) (aplicarEvento unaCiudad (head eventos))
    

--generar Criterior 

compararCantidaDeAtracciones :: Ciudad -> Int
compararCantidaDeAtracciones = length .  atracciones
compararCostoDeVida :: Ciudad -> Float
compararCostoDeVida = costoDeVida
compararValorCiudad :: Ciudad -> Float
compararValorCiudad = valorCiudad 

mejoroCiudadEn :: Ord a => (Ciudad -> a) -> Ciudad -> Evento -> Bool
mejoroCiudadEn unCriterio ciudadInicial evento = unCriterio ciudadInicial < unCriterio ciudadAfectada
    where
        ciudadAfectada = aplicarEvento ciudadInicial evento


aplicarCriteriosSuban :: Ord a => (Ciudad -> a) -> AnioParaRecordar -> Ciudad -> Ciudad
aplicarCriteriosSuban unCriterio (AnioParaRecordar anio eventos) unaCiudad = reflejarPasoAnio (AnioParaRecordar anio soloCriteriosSuban) unaCiudad
    where 
        soloCriteriosSuban = filter (mejoroCiudadEn unCriterio unaCiudad) eventos

aplicarCriteriosBajen :: Ord a => (Ciudad -> a) -> AnioParaRecordar -> Ciudad -> Ciudad
aplicarCriteriosBajen unCriterio (AnioParaRecordar anio eventos) unaCiudad = reflejarPasoAnio (AnioParaRecordar anio soloCriteriosBajen) unaCiudad
    where 
        soloCriteriosBajen = filter (not . mejoroCiudadEn unCriterio unaCiudad) eventos

--PUNTO 6 ------------

esAscendente lista 
    | null lista = False
    | length lista == 1 = True 
    | head lista > (head . tail $ lista) = False
    | otherwise = esAscendente (tail lista) 

estanEventosOrdenados unAnio unaCiudad =  esAscendente . map costoDeVida . map (aplicarEvento unaCiudad) . eventos $ unAnio

esListaCiudadesOrdenada [] evento= False
esListaCiudadesOrdenada (x:[]) evento= True 
esListaCiudadesOrdenada (x:y:[]) evento = costoDeVidaFinal x <= costoDeVidaFinal y
    where 
        costoDeVidaFinal = costoDeVida . ((flip aplicarEvento) evento)
esListaCiudadesOrdenada (x:y:z:xs) evento = costoDeVidaFinal x <= costoDeVidaFinal y && esListaCiudadesOrdenada (y:z:xs) evento

    where 
        costoDeVidaFinal = costoDeVida . ((flip aplicarEvento) evento)
estanAniosOrdenados anios unaCiudad = esAscendente . map costoDeVida . map ((flip reflejarPasoAnio) unaCiudad) $ anios

listaDeEventos :: [Evento]
listaDeEventos = Crisis : Reevaluacion 7 : map Remodelacion [1..]
dosMilVeinticuatro :: AnioParaRecordar
dosMilVeinticuatro = AnioParaRecordar 2024  listaDeEventos

discoRayado :: [Ciudad]
discoRayado = azul : nullish : cycle [caletaOlivia, baradero]
-- ACLARACIONES --
-- fromIntegral :: (Integral a, Num b) => a -> b
-- Toma un numero entero y lo convierte en un número más general. Es útil para trabajar con Integral y Floats juntos.
-- Ejemplo: (4 :: Int) + 3.2 ERROR --> fromIntegral (4 :: Int) + 3.2 Cos


    