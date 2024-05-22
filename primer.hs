import Prelude

double :: Int -> Int 
double number = 2 * number

esNull :: (Eq a, Num a) => a -> Bool
esNull resto = resto == 0


esMultiploDeTres :: Int -> Bool
esMultiploDeTres number= esNull (mod number 3)

esMultiploDe :: Int -> Int -> Bool
esMultiploDe number multiplo= esNull(mod multiplo number)

cubo :: Num a => a -> a 
cubo number = number*number*number

area :: Num a => a -> a -> a
area base altura= base*altura


esBisiesto :: Int -> Bool
esBisiesto anio = not (esMultiploDe 100 anio) && (esMultiploDe 400 anio || esMultiploDe 4 anio)

celsiusToFahr :: Fractional a => a -> a
celsiusToFahr celsius = (celsius * 1.8) + 32

fahrToCelsius :: Fractional a => a -> a
fahrToCelsius fahr = (fahr - 32) * (5/9)

--haceFrio :: (Ord a, Fractional a) => a -> Bool porque debe ser eso
--haceFrio :: Ord a => a -> Bool
haceFrio temp = (fahrToCelsius temp) < 8





--pasar numero 2 y numero 1 a fractional para que luego al devolver devuelva fractional y logre hacer la division 
mcm :: Int -> Int -> Int 
mcm numero1 numero2 =  div (numero1 * numero2) (gcd numero1 numero2)
maxForThree:: Ord a => a -> a -> a -> a
maxForThree number1 number2 number3 = max (max number1 number2) number3

minForThree:: Ord a => a -> a -> a -> a
minForThree number1 number2 number3 = min (min number1 number2) number3

dispersion :: (Num a, Ord a) => a -> a -> a -> a
dispersion level1 level2 level3 = (maxForThree level1 level2 level3) - (minForThree level1 level2 level3)

dispersionChica :: Num a => a
dispersionChica = 30 

dispersionGrande :: Num a => a
dispersionGrande = 100

diasParejos :: (Num a, Ord a) => a -> a -> a -> Bool
diasParejos day1 day2 day3 = (dispersion day1 day2 day3) <= dispersionChica

diasLocos :: (Num a, Ord a) => a -> a -> a -> Bool
diasLocos day1 day2 day3 = (dispersion day1 day2 day3) >= dispersionGrande

diasNormales :: (Num a, Ord a) => a -> a -> a -> Bool
diasNormales day1 day2 day3 = (not (diasLocos day1 day2 day3)) && (not (diasParejos day1 day2 day3))

daysType :: (Num a, Ord a) => a -> a -> a -> String
daysType day1 day2 day3 = if diasParejos day1 day2 day3 then "Dias Parejos" else (if diasLocos day1 day2 day3 then "Dias Locos" else "Dias Normales")

pesoPinoMayor3 :: Num a => a -> a 
pesoPinoMayor3 altura = (3 * 300) + ((altura - 300) * 2)

pesoPinoMenor3 :: Num a => a -> a 
pesoPinoMenor3 altura = altura * 3

pesoPino :: (Num a, Ord a) => a -> a 
pesoPino altura = if altura <= 300 then pesoPinoMenor3 altura else pesoPinoMayor3 altura

esPesoUtil :: (Ord a, Num a) => a -> Bool
esPesoUtil peso = peso >= 400 && peso <= 1000

sirvePino :: (Ord a, Num a) => a -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)

siguienteImpar impar = impar + 2 
--Punto 12 
impar1 = 1
anterior1 = 0

esCuadradoPerfecto :: Integer -> Bool
esCuadradoPerfecto numero = if numero == 0 
                                then True 
                                else esCuadradoAux numero anterior1 impar1

esCuadradoAux :: (Num t, Ord t) => t -> t -> t -> Bool
esCuadradoAux numero anterior impar = if numero == (anterior + impar) 
                                        then True 
                                        else (if numero < (anterior + impar) 
                                                then False 
                                                else  esCuadradoAux numero (anterior + impar) (siguienteImpar impar))
    