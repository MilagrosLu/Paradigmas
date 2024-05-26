import Prelude
import Text.Show.Functions
import Data.List

--LISTA DE LOS QUE LE VAN GANANDO 
-- LENGTH DE LISTA PARA EL PUETSO 
 --12:10 lectura 20 min 
--PUNTO 1--
data Color = Rojo | Azul | Blanco | Negro deriving (Show, Eq)
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
vaTranquilo unAuto unaCarrera = (not . any (estaCerca unAuto)) unaCarrera && puesto unAuto unaCarrera == 1 

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


--Punto 4

type Evento = Carrera -> Carrera 

{-
data Posicion = Posicion{
    posicion :: Int ,
    auto :: Color
} deriving (Show, Eq)
-}
--type TablaPosicion = [(Int, Color)]

simularCarrera :: Carrera -> [Evento] -> [(Int, Color)]
simularCarrera unaCarrera eventos = map (obtenerPosicion carreraFinalizada ) carreraFinalizada
    where   
        carreraFinalizada = foldr (\evento carrera -> evento carrera) unaCarrera eventos

obtenerPosicion :: Carrera -> Auto -> (Int, Color)
obtenerPosicion unaCarrera unAuto = (puesto unAuto unaCarrera , color unAuto )
    -- realizar toda la carrera y luego hgaver un map de los autos resultantes con calculo del puento. 

correnTodos :: Int -> Evento 
correnTodos tiempo = map (correr tiempo) 
usarPowerups :: Color -> PowerUp -> Carrera -> Carrera
usarPowerups colorAuto powerUp unaCarrera = powerUp auto unaCarrera 
    where 
        auto = (\ [x] -> x) . filter ((== colorAuto) . color) $ unaCarrera

-- tirar excepcion si hay más de un auto del mimso color algo que no puede pasar

rojo :: Auto 
rojo = Auto Rojo 120 0

azul :: Auto
azul = Auto Azul 120 0

blanco :: Auto
blanco = Auto Blanco 120 0

negro :: Auto
negro = Auto Negro 120 0

eventosCarrera1 :: [Evento]
eventosCarrera1 = [correnTodos 30 , usarPowerups Azul (jetPack 30), usarPowerups Blanco terremoto, correnTodos 40, usarPowerups Blanco (miguelito 20),usarPowerups Negro (jetPack 6), correnTodos 10 ]

carrera1 :: [Auto]
carrera1 = [rojo, azul, blanco, negro]

simularCarrera1 :: [(Int, Color)]
simularCarrera1 = simularCarrera carrera1 eventosCarrera1

--PUNTO 5 
{-
a. Si es posioble agergar un nuevo Powerup solo sebe añadirse a la solución esa función que 
recibira un color y debolvera el powerUp para activarlo a un determinado auto, 
se usar la funcion usarPorwerUp.-}

{-
b. La funcion puesto no podria evaluarse con una lista infinta de autos ya que no se podria analizar 
la distancias recorridas por todos. 
La funcion vaTranquilo solo podrá devolver un resultado en el caso en que encuentre algún auto
el cual esta cerca lo que permitiria que estaCerca pueda ser evaluada a pesar de ser infinitos autos. 
La composición con not temina dando un False que obliga a funcion a dar False sin tener que analizar 
lo que sigue al &&. Esto es posible por la Lazy Evaluation que tiene haskell que va evaluando 
lo necesario -}
