import Prelude
import Text.Show.Functions
import Data.List
--PUNTO 1
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

grupoCriaturasEj = [siempreDetras, gnomos 10]

dipper = Persona {
    edad = 13, 
    experiencia = 5,
    items = [DisfrasDeOveja, SopladorDeHojas]
}
