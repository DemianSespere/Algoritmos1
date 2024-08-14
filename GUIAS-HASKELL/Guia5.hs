--Dada una lista devuelve su cantidad de elementos 
longitud :: [t] -> Integer 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
---------------------------------------
--Devuelve el ultimo elemento de una lista
ultimo :: [Integer] -> Integer
ultimo [x] = x
ultimo (x:xs) = ultimo xs
---------------------------------------
--Saca el primer elemento de una lista
quitaPrimerElemento :: [t] -> [t]
quitaPrimerElemento (x:xs) = xs
--Saca el ultimo elemento de una lista
principio :: [t] -> [t]
principio [x] = [x]
principio (x:xs) = reverso (quitaPrimerElemento (reverso(x:xs)))
---------------------------------------
--Da vuelta el orden de la lista
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]
---------------------------------------
--Busca un elemento dentro de una lista
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece  _ [] = False
pertenece x (y:ys)  | x == y = True
                    | x /= y = pertenece x ys
---------------------------------------
--Mira si todos sus elementos son iguales
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = x == head xs && todosIguales xs
---------------------------------------
--Mira si todos sus elementos son distintos
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs)   | pertenece x xs = False
                        | otherwise = todosDistintos xs
---------------------------------------                    
--Busca si hay elementos repetidos en una lista
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) = pertenece x xs    
---------------------------------------
--Quitar el primer elemento x de la lista
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys) |  x == y = ys
                |  otherwise = y : quitar x ys 
---------------------------------------
--Quitar todos los elementos x de la lista
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y:ys)    | x == y = quitarTodos x ys
                        | otherwise = y : quitarTodos x ys
---------------------------------------
--Deja una sola aparacion de cada elemento
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = x:eliminarRepetidos(quitarTodos x xs)
---------------------------------------
--Se fija si la primera lista contiene a la segunda lista
contiene :: (Eq t) => [t] -> [t] -> Bool
contiene xs [] = True
contiene xs (y:ys) = pertenece y xs  && contiene xs ys 
--Se fija si tienen los mismos elementos dos listas
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = contiene xs ys && contiene ys xs 
---------------------------------------
--Mira si una lista es capicua 
capicua :: (Eq t) => [t] -> Bool
capicua xs = xs == reverso xs
---------------------------------------
--Suma los elementos de una lista
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria [x] = x 
sumatoria (x:xs) = x + sumatoria xs
---------------------------------------
--Multiplica los elementos de una lista
productoria :: [Integer] -> Integer
productoria [] = 0
productoria [x] = x
productoria (x:xs) = x * productoria xs
---------------------------------------
--Devuelve el maximo de una lista
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs) | x>y = maximo (x:xs)
                | otherwise = maximo (y:xs)
---------------------------------------
--Suma n a cada elemento de la lista
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (x+n):sumarN n xs
---------------------------------------
--Toma el primer elemento de la lsita y se lo suma a toda la lista
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = (x+x):sumarN x xs
---------------------------------------
--Toma el ultimo elemento de la lista y se lo suma a toda la lista
--sumarElUltimo ::  [Integer]-> [Integer]
--sumarElUltimo [x] = [x+x]
--sumarElUltimo (x:xs) = (x+ultimo xs): sumarN (ultimo xs) xs
sumarElUltimo ::  [Integer]-> [Integer]
sumarElUltimo xs = sumarN (ultimo xs) xs 
---------------------------------------
--Deja los elementos pares de una lista
paress :: [Integer] -> [Integer]
paress [] = []
paress (x:xs)   | mod x 2 == 0 = x:paress xs
                | otherwise = paress xs
---------------------------------------
--Deja los elementos multiplos de n 
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs)   | mod x n == 0 = x:multiplosDeN n xs
                        | otherwise = multiplosDeN n xs
---------------------------------------
--Ordena de forma creciente una lista
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar xs = ordenar (quitar (maximo xs) xs) ++ [maximo xs]
---------------------------------------
--Saca espacios contiguos 
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs)  | x==y && x==' ' = sacarBlancosRepetidos (y:xs)
                                | otherwise = x:sacarBlancosRepetidos(y:xs)
---------------------------------------
--Cuenta los espacios de un string
contarEspacios :: [Char] -> Integer
contarEspacios [] = 0
contarEspacios (x:xs)   | x==' ' = 1+ contarEspacios xs
                        | otherwise= contarEspacios xs
--Saca los espacios finales de un string
sacarEspacionFin :: [Char] -> [Char]
sacarEspacionFin [] = []
sacarEspacionFin [x] | x==' ' = []
                        | otherwise= [x]
sacarEspacionFin (x:xs) = x:(sacarEspacionFin xs)
--Saca los espacios iniciales de un string
sacarEspacioInicio :: [Char] -> [Char]
sacarEspacioInicio [] = []
sacarEspacioInicio (x:xs) | x==' ' = sacarEspacioInicio xs
                        |otherwise = (x:xs)
limpiarCadena :: [Char] -> [Char]
limpiarCadena xs = sacarEspacioInicio (sacarEspacionFin(sacarBlancosRepetidos xs))
contarPalabras:: [Char] -> Integer
contarPalabras x = contarEspacios (limpiarCadena x) + 1
---------------------------------------
--Devuelve la primera palabra de un string
primerPalabra :: [Char] -> [Char]
primerPalabra [] = []
primerPalabra (x:xs)    |x==' ' = []
                        |otherwise= x:primerPalabra xs 
--Saca la primera palabra de un string
sacarPrimeraPalabra :: [Char] -> [Char]
sacarPrimeraPalabra [] = []
sacarPrimeraPalabra (x:xs)  | x==' ' = xs
                            | otherwise = sacarPrimeraPalabra xs
--Devuelve todas las palabras de un string en una lista
palabras :: [Char] -> [[Char]]
palabras [] = []
palabras xs = (primerPalabra xs) : palabras (sacarPrimeraPalabra(limpiarCadena xs))
---------------------------------------
palabraMasLargaSinLimpiar :: [Char] -> [Char]
palabraMasLargaSinLimpiar xs    | sacarPrimeraPalabra xs == [] = primerPalabra xs 
                                | length (primerPalabra xs) > length (palabraMasLargaSinLimpiar(sacarPrimeraPalabra xs)) = primerPalabra xs
                                | otherwise = palabraMasLargaSinLimpiar (sacarPrimeraPalabra xs)

f4 x y z | x==y = z
        | x**y == y = x
        | otherwise = y

f5 x y z | x==y = z
        | x**y ==y =  z
        | otherwise = z  