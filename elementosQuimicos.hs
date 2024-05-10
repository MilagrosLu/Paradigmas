import Prelude
import Text.Show.Functions
import Data.List

data Componente = Componente {
    sustancia :: Sustancia,
    cantidad :: Int 
    } deriving(Show,Eq)

data Especie = Metal | NoMetal | Halogeno | GasNoble deriving (Show,Eq)
data Sustancia = Elemento {
    nombre:: String,
    simboloQuimico :: String,
    numeroAtomico :: Int,
    especie :: Especie
} | Compuesto {
    componentes :: [Componente],
    nombre :: String,
    especie :: Especie
} deriving (Show,Eq)

hidrogeno :: Sustancia
hidrogeno = Elemento {
    nombre ="Hidrogeno",
    simboloQuimico ="H",
    numeroAtomico = 1,
    especie =  NoMetal
}
oxigeno :: Sustancia
oxigeno = Elemento {
    nombre ="Oxigeno",
    simboloQuimico ="O",
    numeroAtomico = 8,
    especie =  GasNoble
}

agua :: Sustancia
agua = Compuesto {
    componentes = [Componente hidrogeno 2, Componente oxigeno 1],
    nombre = "Agua",
    especie = NoMetal
}
----------------
-- 2. CONDUCE BIEN 
conduceBienElectricidad :: Sustancia -> Bool
conduceBienElectricidad substance = especie substance == GasNoble

conduceBienCalor :: Sustancia -> Bool
conduceBienCalor substance = especie substance == Halogeno

conduceBien :: Sustancia -> Bool
conduceBien substance = conduceBienCalor substance || conduceBienElectricidad substance
-----------
----- 3. NOMBRE DE UNION

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

nombreUnion :: Sustancia -> String
nombreUnion = (++ "uro") . reverse . dropWhile esVocal . reverse . nombre

-- 4. COMBINAR 2 NOMBRES
combinarNombreSustancias :: Sustancia -> String -> String
combinarNombreSustancias sustancia1 nombreSustancia2 = nombreUnion sustancia1 ++ " de " ++ nombreSustancia2


------------------------
-- 5. MEZCLAR UNA SERIE DE COMPONENTES ENTRE SÍ
nombreMezcla ::  [Sustancia] -> String
nombreMezcla sustancias = foldr combinarNombreSustancias (nombre . last $ sustancias) (init sustancias)

mezclar :: [Componente] -> Sustancia
mezclar componentes = Compuesto {
                            componentes= componentes,
                            nombre = nombreMezcla . map sustancia $ componentes,
                            especie = NoMetal
                            }
    
---componentes para probar función
componente1 = Componente agua 2
componente2 = Componente hidrogeno 3
componente3 = Componente oxigeno 1

-----------------------
--6. OBTNER FORMULA DE UNA SUSTANCIA
unirNombres :: [Componente] -> String
unirNombres componentes = "(" ++ concatMap representacion componentes ++ ")"

representacion :: Componente -> String
representacion componente
    | cantidad componente == 1 = formulaSustancia . sustancia $ componente
    | otherwise = (formulaSustancia . sustancia $ componente) ++ (show . cantidad  $ componente)


formulaSustancia :: Sustancia -> String
formulaSustancia (Elemento _ simb _ _) = simb
formulaSustancia (Compuesto componentes _ _ ) = unirNombres componentes
