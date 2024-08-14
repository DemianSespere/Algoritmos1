{-Ejercicio 1 
problema hayQueCodificar (c: Char, mapeo: seq (Char x Char) ) : Bool
requiere: No hay elementos repetidos entre las primeras componentes de mapeo
requiere: No hay elementos repetidos entre las segundas componentes de mapeo
asegura: res = True <-> c es igual a la primera componente de alguna tupla de mapeo-}
{- LO DEJO PORQUE ES MAS ESPECIFICO QUE EL EJERCICIO 
hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar caracter mapeo = pertenece caracter (listadoprimero mapeo) && hayQueCodificarAux1 (listadoprimero mapeo) && hayQueCodificarAux2 (listadosegundo mapeo)
hayQueCodificarAux1 :: [Char] -> Bool
hayQueCodificarAux1 [] = True
hayQueCodificarAux1 (x:xs)   | pertenece x xs = False
                            | otherwise = hayQueCodificarAux1 xs
hayQueCodificarAux2 :: [Char] -> Bool
hayQueCodificarAux2 [] = True
hayQueCodificarAux2 (x:xs)   | pertenece x xs = False
                            | otherwise = hayQueCodificarAux2 xs
listadoprimero :: [(Char,Char)] -> [Char]
listadoprimero [] = []
listadoprimero ((a,b):xs) = a:listadoprimero xs
listadosegundo :: [(Char,Char)] -> [Char]
listadosegundo [] = []
listadosegundo ((a,b):xs) = b:listadosegundo xs
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs) | t==x = True
                    | True = pertenece t xs
-}
hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar caracter ((a,_):xs) = caracter == a || hayQueCodificar caracter xs 

{-Ejercicio 2
problema cuantasVecesHayQueCodificar (c:Char, frase: seq(Char), mapeo: seq (Char x Char) ) : Z 
requiere: No hay elementos repetidos entre las primeras componentes de mapeo
requiere: No hay elementos repetidos entre las segundas componentes de mapeo
requiere: |frase| > 0
requiere: c pertenece a frase
asegura: (res = 0 y hayQueCodificar {c,mapeo} = False) o (res= cantidad de veces que c aparece en frase y hayQueCodificar {c,mapeo} = True)-
-}

cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar caracter frase mapeo | hayQueCodificar caracter mapeo == False = 0
                                                | hayQueCodificar caracter mapeo = contar caracter frase
contar :: Char -> [Char] -> Int
contar _ [] = 0
contar caracter (x:xs) | caracter==x = 1 + contar caracter xs
                        | otherwise = contar caracter xs

{-Ejercicio 3
problema laQueMasHayQueCodificar (frase: seq(Char), mapeo: seq(Char x Char)) : Char 
requiere: No hay elementos repetidos entre las primeras componentes de mapeo
requiere: No hay elementos repetidos entre las segundas componentes de mapeo
requiere: |frase| > 0
requiere: existe al menos un c que pertenece a frase y hayQueCodificar (c,mapeo)=True
asegura: res= c donde c es el caracter tal que cuantasVecesHayQueCodificar(c,frase,mapeo) es mayor a cualquier otro caracter perteneciente a frase
asegura: Si existen mas de un caracter c que cumple la condicion anterior, devuelve el que aparece primero en frase
-}

laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar frase mapeo = laQueMasHayQueCodificarAux frase frase mapeo
laQueMasHayQueCodificarAux :: [Char] -> [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificarAux [f] _ _ = f
laQueMasHayQueCodificarAux (f:f1:fs) fraseOriginal mapeo 
    | f==f1 = laQueMasHayQueCodificarAux (f:fs) fraseOriginal mapeo --Saco repetido
    | cuantasVecesHayQueCodificar f fraseOriginal mapeo >= cuantasVecesHayQueCodificar f1 fraseOriginal mapeo = laQueMasHayQueCodificarAux (f:fs) fraseOriginal mapeo
    | otherwise = laQueMasHayQueCodificarAux (f1:fs) fraseOriginal mapeo

{-Ejercicio 4
problema codificarFrase (frase: seq(Char), mapeo: seq(Char x Char) ) : seq (Char)
requiere: No hay elementos repetidos entre las primeras componentes de mapeo
requiere: No hay elementos repetidos entre las segundas componentes de mapeo
requiere: |frase| > 0
asegura: |res| = |frase|
asegura: para todo 0<=i<|frase| si hayQueCodificar -}

codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase (f:fs) mapeo | hayQueCodificar f mapeo = obtenerReemplazo f mapeo : codificarFrase fs mapeo
                            | otherwise = f:codificarFrase fs mapeo

obtenerReemplazo :: Char -> [(Char,Char)] -> Char
obtenerReemplazo caracter ((a,reemplazo):xs)| caracter == a = reemplazo
                                            | otherwise = obtenerReemplazo caracter xs 