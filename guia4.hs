{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
import Prelude
import  Text.Show.Functions

lista :: Fractional a => [a]
lista = [2,3,4]



sumarLista :: Num a => [a] -> a
sumarLista = sum;
frecuenciaCardiaca ::[Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

promedioFrecuenciaCardiaca ::(Num a, Fractional a) => [a]-> a

promedioFrecuenciaCardiaca list = sum list / (fromInteger . toInteger . length $ list)

frecuenciaCardiacaMinuto :: Int -> Int

frecuenciaCardiacaMinuto =  (frecuenciaCardiaca !!) . (`div` 10)

index :: Int -> Int
index  = flip div 10

frecuenciasHastaMomento :: Int -> [Int]
frecuenciasHastaMomento min = ((flip take) frecuenciaCardiaca . (+1) . index) min

esCapicua ::  Eq a =>  [[a]] -> Bool


esCapicua list = (reverse . concat) list == concat list

doble = (2*)
triple = (3*)

mejor func1 func2 number =  max (func1 number) (func2 number)

aplicarPar :: (a -> b) -> [a] -> [b]
aplicarPar func lista = map func lista

--ORDEN Y LISTAS 
esMultiploDe :: Int -> Int -> Bool

esMultiploDe numero multiplo =  (==0) . (mod multiplo) $ numero

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno num lista = any (esMultiploDe num) lista

promedios :: (Num a, Fractional a) => [[a]] -> [a]
promedios listaDelistas = map promedioFrecuenciaCardiaca listaDelistas

promedioSinMenoresCuatro :: (Fractional a, Ord a) => [a] -> a
promedioSinMenoresCuatro = promedioFrecuenciaCardiaca . filter (>4)

promediosSinAplazo :: (Fractional a, Ord a) => [[a]] -> [a]
promediosSinAplazo listaDelistas = map promedioSinMenoresCuatro listaDelistas

mejoresNotas :: Ord a => [[a]] -> [a]
mejoresNotas = (map maximum)

aprobo :: (Ord a, Num a) => [a] -> Bool
aprobo = all  ( >= 6 )

--aprobaron devuelve la lista de los alumnos que aprobaromn 
aprobaron :: (Ord a, Num a) => [[a]] -> [[a]]
aprobaron = (filter aprobo)



divisores :: Int -> [Int]
divisores  n = filter (flip esMultiploDe n)  [1..60]

exists :: (a -> Bool) -> [a] -> Bool

exists func list = any func list

hayAlgunNegativo :: (Ord a, Num a) => [a] -> b -> Bool
hayAlgunNegativo lista otraCosa =  any (<0) lista

----10 ??????
-- Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera, devuelve la lista del resultado de aplicar las funciones al valor. P.ej. 
-- Main> aplicarFunciones[(*4),(+3),abs] (-8) 
-- [-32,-5,8] 
-- Si pongo:
-- Main> aplicarFunciones[(*4),even,abs] 8 
-- da error. ¿Por qué? 

aplicarFunciones :: [a -> b] -> a -> [b]
aplicarFunciones listaFunc valor = map ($ valor) listaFunc
-- ????

sumaF :: Num b => [a -> b] -> a -> b
sumaF listaFunc valor = sum . aplicarFunciones listaFunc $ valor

subirHabilidadIndividual :: Int -> Int -> Int
subirHabilidadIndividual aumento habilidad
                                            |(habilidad + aumento) < 12 = habilidad + aumento
                                            | otherwise = 12


subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad aumento habilidades = map (subirHabilidadIndividual aumento) habilidades

flimitada :: (Ord b, Num b) => (a -> b) -> a -> b

flimitada func habilidadVieja
                                |func habilidadVieja > 12 = 12
                                |func habilidadVieja < 0 = 0
                                |otherwise = func habilidadVieja


cambiarHabilidad :: (Ord b, Num b) => (a -> b) -> [a] -> [b]
cambiarHabilidad func listaHabilidades = map (flimitada func) listaHabilidades

minCuatros :: Int -> Int
minCuatros habilidad
                            |habilidad <=4 = 4
                            |otherwise = habilidad

--la funcion takeWhile toma una funcion que devuleve un bool y una lista de elements. devolvera la lista de elementos tomando los elementos en orden parando de tomar cunado se encuentre con el elemento que falle con la condición de la función 
-- ejemplo 
--ghci> takeWhile (==3) [3,3,3,5,2,3,3,3]
--[3,3,3]

esPar :: Int -> Bool
esPar = (==0) . (flip mod) 2

primerosPares :: [Int] -> [Int]
primerosPares  = takeWhile esPar

primerosDivisores :: Int -> [Int] -> [Int]
primerosDivisores n lista = takeWhile ((flip esMultiploDe) n) lista

primerosNoDivisores :: Int -> [Int] -> [Int]
primerosNoDivisores n lista = takeWhile (not . (flip esMultiploDe n)) lista



huboMesMejorDe :: (Ord a, Num a) => [a] -> [a] -> a -> Bool

huboMesMejorDe ingreso egreso comparador = any (>comparador) (zipWith (+) ingreso egreso)

crecimientoAnual :: Int -> Int
crecimientoAnual edad
        | edad < 1 = 0
        | edad < 10 = 24 - (edad *2)
        | edad <=15 = 4
        | edad <=17 = 2
        | edad <= 19 = 1
        | edad >=20 = 0
--se toma que a los 0 años no se crece


auXcrecimeintoEntreEdades :: Int -> Int -> Int -> Int
auXcrecimeintoEntreEdades sumatoria edad1 edad2
        | edad1 == edad2 = sumatoria
        | edad1 < edad2 = auXcrecimeintoEntreEdades (crecimientoAnual edad1 + sumatoria)  (edad1 + 1) edad2

crecimientoEntreEdades:: Int -> Int -> Int
crecimientoEntreEdades = auXcrecimeintoEntreEdades 0

--Nota: Definir la función crecimientoEntreEdades en una sola línea, usando map y suma.

crecimientoEntreEdades' edad1 edad2 = sum . map crecimientoAnual $ [edad1 .. edad2 - 1]


alturasEnUnAnio :: Int -> [Int] -> [Int]
alturasEnUnAnio edad alturas = map (+ crecimientoAnual edad) alturas

alturaEnEdades :: Int -> Int -> [Int] -> [Int]
alturaEnEdades alturaActual edadActual  listaEdades = map  ( (+ alturaActual) . crecimientoEntreEdades' edadActual) listaEdades


--consultar si esta bien 
auxRachasLluvias :: [[Int]] -> [Int] -> [[Int]]
auxRachasLluvias rachasBase listaLluvias
            | null . tomarRacha $ listaLluvias = rachasBase
            | otherwise = auxRachasLluvias (rachasBase ++ [tomarRacha listaLluvias]) (sacarRachaAnotada listaLluvias)
            where
                tomarRacha = (takeWhile (/=0)) . dropWhile (==0)
                sacarRachaAnotada = (dropWhile (/=0)) . dropWhile (==0)

rachasLluvias :: [Int] -> [[Int]]
rachasLluvias = filter (/= []) . auxRachasLluvias [[]]

mayorRachaDeLluvias :: [Int] -> Int
mayorRachaDeLluvias = maximum . (map length) . rachasLluvias

suma :: [Int] -> Int 
suma = foldr (+) 0


 
dispersion :: (Foldable t, Ord a, Num a) => t a -> a
dispersion lista = (foldr (max) 0 lista ) - (foldr1 min lista)