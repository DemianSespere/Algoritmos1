{-En Exactas se está jugando un torneo de fútbol y 
la facultad le pidió a los alumnos de IP programar algunas funcionalidades en Haskell.
Los datos con los que contamos para esto son los nombres de los equipos que participan del torneo,
los nombres de los goleadores de cada uno de dichos equipos, y la cantidad de goles convertidos por esos jugadores.
Los nombres de los equipos y sus respectivos goleadores serán modelados mediante tuplas de tipo (String, String), 
donde la primera componente representa el nombre del equipo, 
y la segunda representa el nombre del goleador de dicho equipo.
En los problemas en los cuales se reciban como parámetros secuencias goleadoresPorEquipo y goles, 
cada posición de la lista goles representará la cantidad de goles obtenidos por el goleador del equipo
que se encuentra en en esa misma posición de goleadoresPorEquipo. 
Por ejemplo, si la lista goleadoresPorEquipo es [("Sacachispas","Robertino Giacomini"), ("Fénix","Matías Domínguez")] y
la lista goles es [3, 5], eso indica que Robertino Giacomini metió 3 goles, y Matías Domínguez 5.

1) problema golesDeNoGoleadores (goleadoresPorEquipo: seq⟨String x String⟩,goles:seq< Z >, totalGolesTorneo: Z) : Z {
    requiere: {equiposValidos(goleadoresPorEquipo)}
    requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
    asegura: {res es la cantidad de goles convertidos en el torneo por jugadores que no son los goleadores de sus equipos-}

golesDeNoGoleadores :: [(String,String)] -> [Int] -> Int -> Int
golesDeNoGoleadores _ _ 0 = 0
golesDeNoGoleadores _ goleadores golestotales = golestotales - (sumagoles goleadores)
sumagoles :: [Int] -> Int
sumagoles [] = 0
sumagoles (x:xs) = x + sumagoles xs

{-2) Equipos Válidos [3 puntos]
problema equiposValidos (goleadoresPorEquipo: seq⟨String x String⟩) : Bool {
requiere: {True}
asegura: {(res = true) <=> goleadoresPorEquipo no contiene nombres de clubes repetidos, 
    ni goleadores repetidos, ni jugadores con nombre de club -}
equiposValidos :: [(String,String)] -> Bool
equiposValidos xs = equiposValidosAux (listado xs)
equiposValidosAux :: [String] -> Bool
equiposValidosAux [] = True
equiposValidosAux (x:xs)| pertenece x xs = False
                        | otherwise = equiposValidosAux xs

listado :: [(String,String)] -> [String]
listado [] = []
listado ((a,b):xs) = a:b: listado xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs)  | t==x = True
                    | otherwise = pertenece t xs 


{-3) Porcentaje de Goles [3 puntos]
problema porcentajeDeGoles (goleador: String, goleadoresPorEquipo: seq⟨String x String⟩,goles:seq< Z >) : R {
    requiere: {La segunda componente de algún elemento de goleadoresPorEquipo es goleador}
    requiere: {equiposValidos(goleadoresPorEquipo)}
    requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {Hay al menos un elemento de goles mayores estricto a 0}
    asegura: {res es el porcentaje de goles que marcó goleador sobre el total de goles convertidos por goleadores-}
{-Para resolver este ejercicio pueden utilizar la siguiente función
que devuelve como Float la división entre dos números de tipo Int: -}
porcentajeDeGoles :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeGoles _ [] _ = 0
porcentajeDeGoles _ _ [0] = 0
porcentajeDeGoles _ _  [g] = 100 
porcentajeDeGoles goleador equipo goles = (division (golesDelGoleador goleador equipo goles) (sumagoles goles)) * 100
golesDelGoleador :: String -> [(String,String)] -> [Int] -> Int
golesDelGoleador goleador (e1:es) (g1:gs) | aparece goleador e1 = g1
                                            | otherwise = golesDelGoleador goleador es gs
aparece :: String -> (String,String) -> Bool
aparece goleador (a,b) = goleador == b
division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

{-4) Botín de Oro [3 puntos]
problema botinDeOro (goleadoresPorEquipo: seq⟨String x String⟩, goles:seq< Z >) : String {
    requiere: {equiposValidos(goleadoresPorEquipo)}
    requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {|goles| > 0}
    asegura: {res es alguno de los goleadores de goleadoresPorEquipo que más tantos convirtió de acuerdo a goles-}

botinDeOro :: [(String,String)] -> [Int] -> String
botinDeOro _ [0] = "converti un gol muerto"
botinDeOro [(e,g)] _ = g
botinDeOro ((e1,j1):(e2,j2):ejs) (g1:g2:gs) | g1>g2 = botinDeOro ((e1,j1):ejs) (g1:gs)
                                            | otherwise = botinDeOro ((e2,j2):ejs) (g2:gs)