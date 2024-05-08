import Prelude
import Text.Show.Functions
import Data.List

data Componente = Comp {
    sustancia :: Sustancia,
    cantidad :: Int 
    } deriving(Show,Eq)

data Especie = Metal | NoMetal | Halogeno | GasNoble deriving (Show,Eq)
data Sustancia = Elementos {
    nombre:: String,
    simboloQuimico :: String,
    numAtomico :: Int,
    grupo :: Especie
} | Compuestos {
    serieComponentes :: [Componente],
    nombre :: String,
    grupo :: Especie
} deriving (Show,Eq)

hidrogeno :: Sustancia
hidrogeno = Elementos {
    nombre ="Hidrogeno",
    simboloQuimico ="H",
    numAtomico = 1,
    grupo =  NoMetal
}
oxigeno :: Sustancia
oxigeno = Elementos {
    nombre ="Oxigeno",
    simboloQuimico ="O",
    numAtomico = 8,
    grupo =  GasNoble
}

agua :: Sustancia
agua = Compuestos {
    serieComponentes= [Comp hidrogeno 2, Comp oxigeno 1],
    nombre = "Agua",
    grupo = NoMetal
}
----------------
-- 2. CONDUCE BIEN 
conduceBienElectricidad :: Sustancia -> Bool
conduceBienElectricidad substance = grupo substance == GasNoble

conduceBienCalor :: Sustancia -> Bool
conduceBienCalor substance = grupo substance == Halogeno

conduceBien :: Sustancia -> Bool
conduceBien substance = conduceBienCalor substance || conduceBienElectricidad substance
-----------
----- 3. NOMBRE DE UNION
esVocal :: Char -> Bool
esVocal letra = elem letra "aeiou"

sufijoUnion :: String
sufijoUnion = "uro"

sacarUltimoElemento :: [a] -> [a]
sacarUltimoElemento lista =  reverse . tail . reverse $ lista

auxNombreUnion :: String -> String
auxNombreUnion nomsust
                | not . esVocal . last $ nomsust = nomsust ++ sufijoUnion
                | esVocal . last $ nomsust = auxNombreUnion . sacarUltimoElemento $ nomsust

nombreUnion :: Sustancia -> String
nombreUnion sust = auxNombreUnion . nombre $ sust
---------------
-- 4. COMBINAR 2 NOMBRES
combinarSustanciasAux :: String -> String -> String
combinarSustanciasAux nomsust1 nomsust2 = auxNombreUnion nomsust1 ++ " de " ++ nomsust2

combinarSustancias :: Sustancia -> Sustancia -> String
combinarSustancias sust1 sust2 = combinarSustanciasAux (nombre sust1) (nombre sust2)

------------------------
-- 5. MEZCLAR UNA SERIE DE COMPONENTES ENTRE SÍ
combinarNombres ::  [String] -> String
combinarNombres listaNombres = foldr combinarSustanciasAux (last listaNombres) (sacarUltimoElemento listaNombres)

mezclar :: [Componente] -> Sustancia
mezclar listaComponentes = Compuestos {
                            serieComponentes= listaComponentes,
                            nombre = combinarNombres . map (nombre . sustancia) $ listaComponentes,
                            grupo = NoMetal
                            }
    
---componentes para probar función
componente1 = Comp agua 2
componente2 = Comp hidrogeno 3
componente3 = Comp oxigeno 1

-----------------------
--6. OBTNER FORMULA DE UNA SUSTANCIA
juntarSustancia :: [Componente] -> String
juntarSustancia component = "(" ++ concatMap concatenarComponentes component ++ ")"

concatenarComponentes :: Componente -> String
concatenarComponentes (Comp (Elementos _ sust _ _) cantidad)
    | cantidad == 1 = sust
    | otherwise = sust ++ show cantidad 

concatenarComponentes (Comp (Compuestos comp _ _ ) cantidad)
    | cantidad == 1 = juntarSustancia comp
    | otherwise = juntarSustancia comp ++ show cantidad

obtenerFormula :: Sustancia -> String
obtenerFormula (Elementos _ simb _ _) = simb
obtenerFormula (Compuestos comp _ _ ) = juntarSustancia comp