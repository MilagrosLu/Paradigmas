import Prelude
import  Text.Show.Functions

siguiente :: Int -> Int 

siguiente numero = numero + 1

mitad :: Fractional a => a -> a

mitad numero = numero / 2

inversa :: Fractional a => a -> a 

inversa numero = 1 / numero 

triple :: Num a => a -> a

triple numero = 3 * numero 

esNumeroPositivo :: ( Num a , Fractional a , Ord a) => a -> Bool 
esNumeroPositivo numero = numero >= 0 



esMultiploDe :: Int -> Int -> Bool 

esMultiploDe numero multiplo =  (==0) . (mod multiplo) $ numero


-- esBisiesto anio = not . esMultiploDe 100 $ anio) && (esMultiploDe 400 anio || esMultiploDe 4 anio)
esBisiesto :: Int -> Bool 
esBisiesto anio =  ((not . esMultiploDe 100 $ anio) &&) . (esMultiploDe 400 anio ||) . esMultiploDe 4 $ anio

-- esta bien aplicado??? 

inversaRaizCuadrada :: (Fractional a, Floating a) => a -> a
inversaRaizCuadrada numero = inversa . sqrt $ numero 

cuadradoN :: Num a => a -> a
cuadradoN n = n * n

incrementMCuadradoN :: Num a => a -> a -> a 
incrementMCuadradoN m n = (n +) . cuadradoN $ m

esResultadoPar :: Int -> Int -> Bool
esResultadoPar m n =  (==0) . (`mod` 2) . (m^) $ n 