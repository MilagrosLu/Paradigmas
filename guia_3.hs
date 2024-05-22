import Prelude
import  Text.Show.Functions

fst3 :: (a, b, c) -> a
fst3 (primero , _ , _) = primero

snd3 :: (a, b, c) -> b
snd3 ( _ , segundo , _)= segundo

thr3 :: (a, b, c) -> c
thr3 ( _ , _, tercero) = tercero

aplicar :: (Int -> a, Int -> b) -> Int -> (a,b)
aplicar (funcion1, funcion2 ) numero = (funcion1 numero , funcion2 numero)

cuentaBizarra (a,b)
    | a > b = a + b
    | b - a >= 10  = b - a
    | b - a <= 10 && b > a = b * a
esNotaBochazo = (< 6)

aprobo :: (Integer, Integer) -> Bool
aprobo (nota1, nota2 ) = not (esNotaBochazo nota1 || esNotaBochazo nota2)


promociono (nota1, nota2) = nota1 + nota2 >= 15 && esNotaParaPromocionar nota1 && esNotaParaPromocionar nota2
    where
        esNotaParaPromocionar  = (>=7)

aproboPrimerParcial (nota1 , _) = not . esNotaBochazo $ nota1

notasFinales ((parcial1,parcial2),(recuperatorio1,recuperatorio2)) = (max parcial1 recuperatorio1 , max parcial2 recuperatorio2)

--- 5.b 
recuperoPrimerParcial = (/= -1) . fst . snd

recuperoDeGusto :: (Num a, Eq a) => (( Integer, Integer),(a , a)) -> Bool

recuperoDeGusto notas = (promociono . fst $ notas) && (rindioAlgunRecuperatorio . snd $ notas)

rindioAlgunRecuperatorio :: (Eq a1, Eq a2, Num a1, Num a2) => (a1, a2) -> Bool
rindioAlgunRecuperatorio notasRecuparatorios = fst notasRecuparatorios /= -1 || snd notasRecuparatorios /= -1

esMayordeEdad :: (Ord a1, Num a1) => (String, a1) -> Bool
esMayordeEdad =  (>= 21) . snd

calcular :: (Integer, Integer) -> (Integer, Integer)
calcular = calcularSegundoElemento . calcularPrimerElemento

esPar :: Integer -> Bool
esPar = (== 0) . flip mod 2
calcularPrimerElemento (a, b)
    | esPar  a = (2*a ,b)
    | otherwise = (a,b)

calcularSegundoElemento :: (a, Integer) -> (a, Integer)
calcularSegundoElemento (a, b)
    | not . esPar $ b = ( a, b+1)
    | otherwise = (a , b)

doble :: Num a => a -> a
doble a = 2 * a