import Prelude
import Text.Show.Functions
import Data.List

--LISTA DE LOS QUE LE VAN GANANDO 
-- LENGTH DE LISTA PARA EL PUETSO 
 --12:10 lectura 20 min 
--PUNTO 1--
data Color = Rojo | Azul deriving (Show, Eq)
data Auto = Auto {
    color :: Color, 
    velocidad :: Int , 
    distancia :: Int

} deriving (Show, Eq)

type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 =  color auto1 /= color auto2 && abs (distancia auto1 - distancia auto2) < 10

autosQueLeGanan :: Auto -> Carrera -> [Auto]
autosQueLeGanan unAuto = filter ((distancia unAuto <) . distancia) 

puesto :: Auto -> Carrera -> Int
puesto unAuto = (1 +) . length . autosQueLeGanan unAuto 

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = all (not . estaCerca unAuto)  unaCarrera && puesto unAuto unaCarrera == 1 

type NuevoAuto = Auto -> Auto 
--PUNTO 2   
correr :: Int -> NuevoAuto
correr tiempo unAuto = unAuto { distancia = distancia unAuto + velocidad unAuto * tiempo }

alterarVelocidad :: (Int -> Int) -> NuevoAuto
alterarVelocidad modificador unAuto = unAuto {velocidad = modificador (velocidad unAuto)}


bajarVelocidad :: Int -> NuevoAuto
bajarVelocidad cantidad = alterarVelocidad (max 0 . flip (-) cantidad) 

--PUNTO 3

type PowerUp = Auto  -> Carrera -> Carrera 

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: PowerUp
terremoto autoGatillo = afectarALosQueCumplen (estaCerca autoGatillo) (bajarVelocidad 50) 

miguelito :: Int -> PowerUp
miguelito cantidad autoGatillado = afectarALosQueCumplen ((distancia autoGatillado >) . distancia) (bajarVelocidad cantidad)

jetPack :: Int -> PowerUp
jetPack duracion autoGatillado = afectarALosQueCumplen ((color autoGatillado ==) . color ) (efectoJetPack duracion) 
    where 
        efectoJetPack duracion unAuto =  alterarVelocidad (const (velocidad unAuto))  . correr duracion . alterarVelocidad (*2) $  unAuto

