module Simulacro where

{-problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
requiere: {True}
asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
1 A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.-}

sonIguales :: [(String,String)] -> Bool
sonIguales [] = False
sonIguales ((a,b):xs)   | a==b = True
                        | otherwise = sonIguales xs
relacionRepetida :: [(String,String)] -> Bool
relacionRepetida [] = False
relacionRepetida [x] = False
relacionRepetida ((a,b):(c,d):xs)   | a==c && b==d || a==d && b==c = True
                                    | relacionRepetida ((a,b):xs) = True
                                    | otherwise = relacionRepetida ((c,d):xs)
relacionesValidas :: [(String,String)] -> Bool
relacionesValidas xs = not(sonIguales xs) && not(relacionRepetida xs)

{-problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
requiere: {relacionesValidas(relaciones)}
asegura: {res no tiene elementos repetidos}
asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones-}

todasLasPersonas :: [(String,String)] -> [String]
todasLasPersonas [] = []
todasLasPersonas ((a,b):xs) = a:b:todasLasPersonas xs
quitarTodos :: (Eq t) => (t) -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y:ys)| x==y = quitarTodos x ys
                    | otherwise= y : quitarTodos x ys
personasRepetidas :: [String] -> [String]
personasRepetidas [] = []
personasRepetidas [x] = [x]
personasRepetidas (x:xs) = x: personasRepetidas(quitarTodos x xs)
personas :: [(String,String)] -> [String]
personas xs = personasRepetidas (todasLasPersonas xs)
{-problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
requiere: {relacionesValidas(relaciones)}
asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona-}
amigosDeAux1 :: (String) -> [(String,String)] -> [(String,String)]
amigosDeAux1 _ [] = []
amigosDeAux1 p ((a,b):xs)   | p==a || p==b = (a,b):amigosDeAux1 p xs
                            | otherwise = amigosDeAux1 p xs
amigosDeAux2 :: (String) -> [(String,String)] -> [String]
amigosDeAux2 _ [] = []
amigosDeAux2 p ((a,b):xs)   | p==a = b:amigosDeAux2 p xs
                            | otherwise= a:amigosDeAux2 p xs
amigosDe :: (String) -> [(String,String)] -> [String]
amigosDe p xs = amigosDeAux2 p (amigosDeAux1 p xs) 

{-problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
requiere: {relaciones no vacía}
requiere: {relacionesValidas(relaciones)}
asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)-}
--personaConMasAmigos :: [(String,String)] -> (String)
personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos relaciones = personaConMasAmigosAux (personas relaciones) "nadie" relaciones
personaConMasAmigosAux :: [String] -> String -> [(String,String)] -> String
personaConMasAmigosAux [] masAmistoso _ = masAmistoso
personaConMasAmigosAux (p1:ps) masAmistoso relaciones 
    | longitud(amigosDe p1 relaciones)>= longitud(amigosDe masAmistoso relaciones) = personaConMasAmigosAux ps p1 relaciones
    | otherwise = personaConMasAmigosAux ps masAmistoso relaciones
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
