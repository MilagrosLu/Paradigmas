import Prelude
import Text.Show.Functions
import Data.List
--PUNTO 1 en 1hs 15 min
data Item = DisfrasDeOveja | SopladorDeHojas deriving (Show, Eq)
data Persona = Persona {
    edad :: Int, 
    experiencia :: Int, 
    items :: [Item]
} deriving (Show, Eq)

data Criatura = Criatura {
    peligrosidad :: Int ,
    condicionesDeshacerse :: [Condiciones]
} deriving (Show )

type Condiciones = Persona -> Bool

siempreDetras :: Criatura
siempreDetras = Criatura { 
    peligrosidad = 0 ,
    condicionesDeshacerse = []
}
gnomos :: Int -> Criatura
gnomos 1 = Criatura {
    peligrosidad = 0, 
    condicionesDeshacerse = [tieneItem SopladorDeHojas]
}
gnomos cantidad = Criatura {
    peligrosidad = 2 ^ cantidad, 
    condicionesDeshacerse = [tieneItem SopladorDeHojas]
}

tieneItem :: Item -> Persona -> Bool
tieneItem unItem = elem unItem . items 

modificarExperiencia :: (Int -> Int) -> Persona -> Persona
modificarExperiencia modificador unaPersona = unaPersona { experiencia = modificador (experiencia unaPersona) }

fantasmas :: Int -> [Condiciones] -> Criatura
fantasmas categoria asuntosPendientes = Criatura {
    peligrosidad = categoria * 20,
    condicionesDeshacerse = asuntosPendientes
}
--PUNTO 2
personaCumpleCondiciones :: [Condiciones] -> Persona -> Bool
personaCumpleCondiciones [] unaPersona = False
personaCumpleCondiciones unasCondiciones unaPersona =  all cumpleCondicion unasCondiciones
    where 
        cumpleCondicion condicion = condicion unaPersona

enfrentarseCriatura :: Criatura -> Persona -> Persona
enfrentarseCriatura (Criatura peligrosidad unasCondiciones) unaPersona 
    | personaCumpleCondiciones unasCondiciones unaPersona = modificarExperiencia (+ peligrosidad) unaPersona
    | otherwise = modificarExperiencia (+ 1) unaPersona

--PUNTO 3 
experienciaGanada :: [Criatura] -> Persona -> Int 

experienciaGanada unasCriaturas unaPersona = experiencia (enfrentarseGrupoCriaturas unasCriaturas unaPersona)

enfrentarseGrupoCriaturas :: [Criatura] -> Persona -> Persona 
enfrentarseGrupoCriaturas unasCriaturas unaPersona = foldr enfrentarseCriatura unaPersona unasCriaturas

tenerEdad :: (Int -> Bool) -> Persona -> Bool
tenerEdad unaCondicion = unaCondicion  . edad

tenerExperiencia :: (Int -> Bool) -> Persona -> Bool
tenerExperiencia unaCondicion = unaCondicion . experiencia

grupoCriaturas1 :: [Criatura]
grupoCriaturas1 = [siempreDetras, gnomos 10, fantasmas 3 [tenerEdad (<13), tieneItem DisfrasDeOveja] , fantasmas 1 [tenerExperiencia (>10)]]

grupoCriaturasEj :: [Criatura]
grupoCriaturasEj = [siempreDetras, gnomos 10]

dipper :: Persona
dipper = Persona {
    edad = 13, 
    experiencia = 5,
    items = [DisfrasDeOveja]
}

--PARTE 2 
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
zipWithIf _ _ [] [] = []
zipWithIf _ _ (x:xs) [] = []
zipWithIf _ _ [] (y:ys) = []
zipWithIf operacion unaCondicion (x:xs) (y:ys) 
    | not . unaCondicion $ y = y : zipWithIf operacion unaCondicion (x:xs) ys 
    | otherwise = operacion x y :  zipWithIf operacion unaCondicion xs ys 

abecedario :: [Char]
abecedario = ['a'..'z']

abecedarioDesde :: Char -> [Char]
abecedarioDesde letraComienzo = dropWhile (/= letraComienzo) abecedario ++ init ['a' .. letraComienzo ]

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave letraEncriptada = fst  . head . dropWhile ((/= letraEncriptada) . snd) .  zip abecedario . abecedarioDesde $ letraClave

listaClaveRepetida :: String -> String -> String 
listaClaveRepetida clave mensajeEncriptado = concat (replicate (div (length mensajeEncriptado) (length clave) + 1) clave) 

desencriptarFrase :: [Char] -> [Char] -> [Char]
desencriptarFrase clave mensajeEncriptado = zipWithIf desencriptarLetra (`elem` abecedario) (listaClaveRepetida clave mensajeEncriptado) mensajeEncriptado

vigenere :: String -> String -> String 
vigenere  = desencriptarFrase 

cesar :: Char -> String -> String
cesar letraClave  = desencriptarFrase [letraClave]

todasLasDesencriptaciones :: String -> [String]
todasLasDesencriptaciones unMensaje = map (`cesar` unMensaje) abecedario