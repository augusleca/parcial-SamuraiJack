module Library where
import PdePreludat

data Elemento = UnElemento { 
    tipo :: String
,   ataque :: (Personaje-> Personaje)
,   defensa :: (Personaje-> Personaje)
}deriving (Show,Eq)

data Personaje = UnPersonaje {
  nombre :: String
, salud :: Number
, elementos :: [Elemento]
, anioPresente :: Number
}deriving (Show,Eq)

type Victima = Personaje
type Defensa = Personaje -> Personaje
type Ataque = Personaje -> Personaje

ranuraElementoVacia :: Personaje -> Personaje
ranuraElementoVacia = id

-- 1)

titazo = UnPersonaje "Augusto" 2000 [katana,katana] 2002
russianBoy = UnPersonaje "Alexander" 2000 [concentracion 3] 1945
linoneta = UnPersonaje "Marcelo" 50 [] 2021

--a)

mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio anioIndicado personaje = personaje {anioPresente = anioIndicado }

meditar :: Number -> Personaje -> Personaje
meditar valor = modificarSalud ((valor/2))

causarDanio :: Number -> Personaje -> Personaje
causarDanio cantDanio personaje = modificarSalud (max (-cantDanio) (-salud personaje)) personaje

modificarSalud :: Number -> Personaje -> Personaje
modificarSalud cantSalud personaje = personaje {salud = (salud personaje) + cantSalud}

-- 2)

--a)
esMalvado :: Personaje -> Bool
esMalvado personaje = algunElementoMalvado (elementos personaje)

algunElementoMalvado :: [Elemento] -> Bool
algunElementoMalvado elementos = any elementoEsMalvado elementos

elementoEsMalvado :: Elemento -> Bool
elementoEsMalvado elemento = tipo elemento == "Maldad"

--b) 
danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personaje elemento = (salud personaje) - (salud (flip aplicarAtaque personaje elemento))

--c)
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (hayAlgunElementoMortal personaje) enemigos

hayAlgunElementoMortal :: Personaje -> Personaje -> Bool
hayAlgunElementoMortal personaje enemigo = any (elementoLograMatar personaje) (elementos enemigo)

elementoLograMatar :: Personaje -> Elemento -> Bool
elementoLograMatar personaje elemento = salud (aplicarAtaque elemento personaje) == 0

-- 3)

--a)
aplicarMeditarNVeces :: Number -> Personaje -> Personaje
aplicarMeditarNVeces nivelConcentracion personaje = foldr id personaje (cuantasMeditaciones nivelConcentracion) 

cuantasMeditaciones :: Number -> [Personaje -> Personaje]
cuantasMeditaciones nivelConcentracion = take nivelConcentracion (repeat (meditar 10)) 

--b)
esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidadEsbirros = take cantidadEsbirros (repeat esbirros)

esbirros = UnElemento "Maldad" ataqueEsbirro ranuraElementoVacia

ataqueEsbirro :: Personaje -> Personaje
ataqueEsbirro = causarDanio 1

--c)
concentracion nivelConcentracion = UnElemento "Magia" ranuraElementoVacia (aplicarMeditarNVeces nivelConcentracion)
katana = UnElemento "Magia" (causarDanio 1000) ranuraElementoVacia

jack = UnPersonaje "Jack" 300 [concentracion 3, katana] 200

--d)
aku :: Number -> Number -> Personaje
aku añoIndicado saludInicial = UnPersonaje "EnemigoX" saludInicial ([concentracion 4]++(esbirrosMalvados (100*añoIndicado))++[portalAlFuturo añoIndicado saludInicial]) añoIndicado

portalAlFuturo :: Number -> Number -> Elemento
portalAlFuturo añoIndicado saludInicial = UnElemento "Magia" (mandarAlAnio (añoIndicado + 2800)) (aku añoIndicado.salud)

-- 4)

luchar :: Personaje -> Personaje -> (Personaje,Personaje)
luchar atacante defensor
  | atacanteMuerto (atacante,defensor) = (defensor,atacante)
  | otherwise = luchar (aplicarElementosPersonaje atacante defensor) (aplicarElementosPersonaje defensor atacante)

atacanteMuerto :: (Personaje, Personaje) -> Bool
atacanteMuerto (atacante,defensor) = (salud atacante) == 0

aplicarElementosPersonaje :: Personaje -> Personaje -> Personaje
aplicarElementosPersonaje atacante defensor = aplicarAtaquesConsecutivos atacante (aplicarDefensasConsecutivas defensor)

aplicarDefensasConsecutivas :: Personaje -> Personaje
aplicarDefensasConsecutivas personaje = foldr aplicarDefensa personaje (elementos personaje)

aplicarAtaquesConsecutivos :: Personaje -> Victima -> Victima
aplicarAtaquesConsecutivos personaje victima = foldr aplicarAtaque victima (elementos personaje) 

aplicarAtaque :: Elemento -> Victima -> Victima
aplicarAtaque elemento = ataque elemento

aplicarDefensa :: Elemento -> Personaje -> Personaje
aplicarDefensa elemento = defensa elemento

