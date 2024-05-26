import Prelude
import Text.Show.Functions
import Data.List


--PUNTO 1---- 

data Turista = Turista {
    stress :: Int, 
    cansancio :: Int, 
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show , Eq)
 
ana :: Turista 
ana = Turista {
    stress = 21,
    cansancio= 0,
    viajaSolo = False, 
    idiomas = ["Espanol"]

}

beto :: Turista
beto = Turista 15 15 True ["Aleman"]
cathi :: Turista
cathi = Turista 15 15 True ["Aleman","Catalan"]

--PUNTO 2---
type Excursion = Turista -> Turista

data Marea = Fuerte | Moderada | Tranquila

restarHastaCero :: Int -> Int
restarHastaCero = max 0

agregarIdioma :: String -> Turista -> Turista
agregarIdioma idioma unTurista = unTurista {idiomas = idioma : idiomas unTurista}
modificarStress :: Int -> Turista -> Turista
modificarStress numero unTurista = unTurista {stress = max 0 (stress unTurista + numero)}

modificarCansancio :: Int -> Turista -> Turista
modificarCansancio  numero unTurista = unTurista {cansancio = max 0 (cansancio unTurista + numero)}

viajarAcompaniado :: Turista -> Turista
viajarAcompaniado unTurista = unTurista { viajaSolo = False}

salirAHablarIdioma :: String -> Excursion
salirAHablarIdioma idioma unTurista 
    | idioma `elem` idiomas unTurista =  viajarAcompaniado unTurista
    | otherwise= agregarIdioma idioma . viajarAcompaniado $ unTurista

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista  = modificarCansancio (-5) unTurista
    | otherwise = modificarStress (-5) unTurista

apreciarElementoPaisaje :: String -> Excursion
apreciarElementoPaisaje elemento = modificarStress (-(length elemento))

caminarCiertosMinutos :: Int -> Excursion
caminarCiertosMinutos minutos =  modificarStress (-intensidadCaminata) .  modificarCansancio intensidadCaminata 
    where
        intensidadCaminata = div minutos 4 

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte unTurista =  modificarStress 6 .  modificarCansancio 10 $ unTurista
paseoEnBarco Moderada unTurista = unTurista 
paseoEnBarco Tranquila unTurista =  salirAHablarIdioma "Aleman" . apreciarElementoPaisaje "mar". caminarCiertosMinutos 10 $ unTurista


hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion unTurista = excursion (modificarStress (negate (div 10 100) * stress unTurista) unTurista)


--PUNTO 2 .B ---

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Excursion -> Turista -> Int 
deltaExcursionSegun indice excursion unTurista = deltaSegun indice turistaDespuesExcursion unTurista
    where 
        turistaDespuesExcursion = hacerUnaExcursion excursion unTurista

esExcursionEducativa :: Excursion -> Turista -> Bool

esExcursionEducativa unaExcursion unTurista = deltaExcursionSegun (length . idiomas) unaExcursion unTurista > 0

esExcursionDesestresante :: Turista -> Excursion -> Bool
esExcursionDesestresante unTurista unaExcursion = deltaExcursionSegun stress unaExcursion unTurista <= -3

excursionDesestresante :: [Excursion] -> Turista -> [Excursion]
excursionDesestresante excursiones unTurista = filter (esExcursionDesestresante unTurista) excursiones

--PUNTO 3 -- 
type Tour = [Excursion]

tourCompleto :: Tour
tourCompleto = [caminarCiertosMinutos 20, apreciarElementoPaisaje "cascada", caminarCiertosMinutos 40, irALaPlaya, salirAHablarIdioma "melmacquiano"]

tourLadoB :: Excursion -> Tour
tourLadoB excursionElegida= [paseoEnBarco Tranquila , excursionElegida , caminarCiertosMinutos 120]

tourIslaVecina :: Marea -> Tour
tourIslaVecina Fuerte = [paseoEnBarco Fuerte, apreciarElementoPaisaje "lago", paseoEnBarco Fuerte]
tourIslaVecina Tranquila = [paseoEnBarco Tranquila , irALaPlaya , paseoEnBarco Tranquila ]
tourIslaVecina Moderada = [paseoEnBarco Moderada , irALaPlaya , paseoEnBarco Moderada]


hacerTour :: Tour -> Turista -> Turista
hacerTour unTour unTurista = foldr hacerUnaExcursion (modificarStress cantidadExcursiones unTurista) unTour
    where 
        cantidadExcursiones = length unTour

hayToursConvincentes :: [Tour] -> Turista -> Bool
hayToursConvincentes tours unTurista = any (`esTourConvincente` unTurista) tours

esTourConvincente :: Tour -> Turista -> Bool
esTourConvincente tour unTurista= any (esExcursionDesestresante unTurista) tour && finalizaAcompaniado tour unTurista

finalizaAcompaniado :: Tour -> Turista -> Bool
finalizaAcompaniado tour unTurista = not . viajaSolo $ hacerTour tour unTurista 

efectividadTour :: Tour -> [Turista] -> Int
efectividadTour tour turistas = sum . map (espiritualidad tour) $ turistasConsideranConvincente
    where 
        turistasConsideranConvincente = filter (esTourConvincente tour) turistas

sumaPerdidaIndice :: (Turista -> Int) -> Tour -> Turista -> Int
sumaPerdidaIndice _ [] unTurista = 0
sumaPerdidaIndice indice (x:xs) unTurista 
    | deltaExcursionSegun indice x unTurista <0 = (-1) * deltaExcursionSegun indice x unTurista + sumaPerdidaIndice indice xs unTurista
    | otherwise = sumaPerdidaIndice indice xs unTurista
  
espiritualidad :: Tour -> Turista -> Int
espiritualidad tour unTurista = sumaPerdidaIndice stress tour unTurista + sumaPerdidaIndice cansancio tour unTurista


--PUNTO 4-- 

tourInfinitasPlayas :: Tour
tourInfinitasPlayas = repeat irALaPlaya


{-B. Por como esta modelada la función, debe analizar si termina el tour acomañado, 
por lo que debe analizar todo el tour, siendo imposible ya que el tour tiene infinitas excursiones. -}

{-c. no es posible saber la espiritualidad del tour ya que debe sumar todas las peridas de stress sus excurciones , las 
cuales son infinitas. El único caso en el que se podrá obenter un resultado es si se pide la efectividad de una 
lista vacia de turistas -}



