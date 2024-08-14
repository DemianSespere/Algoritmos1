module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: { EQUIPO DINAMITA }
-- Integrante1: { 45399106, Pondal Benjamin }
-- Integrante2: { 43444522, Mereles Matias }
-- Integrante3: { 44577349, Sespere Demian }
-- Integrante4: { 93741108, Chamorro David }
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1
-- Si char es alguno de los caracteres validos devuelve true
esMinuscula :: Char -> Bool
esMinuscula char = char `elem` caracteresValidos

-- Definimos los caracteres que son validos
caracteresValidos :: String
caracteresValidos = minusculasAscii 97 -- Comienza desde el 97 = 'a' y va hasta la 'z'

-- Funcion que arma una lista con caracteres del ascii
minusculasAscii :: Int -> [Char]
minusculasAscii indice
    | indice <= 122 = chr indice : pasoRecursivo
    | otherwise = []
    where pasoRecursivo = minusculasAscii (indice + 1)

-- EJ 2
-- Ord convierte el caracter a su codigo de ascii
-- Ej: ord a = 97 y le restamos 97 para que 'a' sea la posicion 0
letraANatural :: Char -> Int
letraANatural char = ord char - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar char clave 
    | not (esMinuscula char) = char
    | otherwise = caracterDesplazado
    where 
        posChar = letraANatural char
        posFueraDeRango = posChar + clave
        -- Nuestra posicion debe estar entre 0 y 25
        -- Al desplazarla puede ser < 0 o > 25. Para que caiga en el rango usaremos modulo 26 y el resultado estara entre 0 y 25 siempre
        posDesplazada = mod posFueraDeRango 26
        -- Le sumamos 97 para obtener el caracter con el desplazamiento que le aplicamos
        caracterDesplazado = chr (posDesplazada + 97)

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar string clave = desplazarString string clave

-- Desplaza el caracter si es minuscula y lo une a la lista de caracteres desplazados
-- En caso de no ser minuscula, agrega el caracter sin el desplazamiento a la lista de caracteres desplazados
desplazarString :: String -> Int -> String
desplazarString [] _ = []
desplazarString (c:chars) clave
    | not (esMinuscula c) = c : pasoRecursivo
    | otherwise = charDesplazado : pasoRecursivo
    where pasoRecursivo = desplazarString chars clave
          charDesplazado = desplazar c clave

-- EJ 5
-- Descifrar es aplicar la inversa de cifrar
-- Es decir, si al cifrar desplazamos por 2, al descifrar estamos cifrando por -2
descifrar :: String -> Int -> String
descifrar cifrado clave = desplazarString cifrado (-clave)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista lista = cifrarListaAux lista 0

-- Esta funcion mantiene la posicion de cada item de la lista 
-- y la utiliza para cifrar cada elemento con dicha posicion como clave
cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (s:ls) clave = sCifrado : pasoRecursivo
    where 
        sCifrado = desplazarString s clave
        pasoRecursivo = cifrarListaAux ls (clave + 1)

-- EJ 7
frecuencia :: String -> [Float]
frecuencia string = porcentajeLista listaDeApariciones longitudStringMinusculas
    where 
        -- Filtra las no minusculas del string
        stringMinusculas = eliminarNoMinusculas string caracteresValidos
        listaDeApariciones = contarTodasApariciones stringMinusculas caracteresValidos
        longitudStringMinusculas = length stringMinusculas

-- Divide por el total de caracteres
dividir :: Int -> Int -> Float
dividir n total = fromIntegral n / fromIntegral total

-- Divide todos los elementos de una lista y los multiplica por 100
porcentajeLista :: [Int] -> Int -> [Float]
porcentajeLista [] _ = []
porcentajeLista (x:xs) 0 = 0.0 : porcentajeLista xs 0 -- Esto evita la division por 0
porcentajeLista (x:xs) divisor = divX : divXs
    where
        divX = dividir x divisor * 100
        divXs = porcentajeLista xs divisor

-- Devuelve una lista con la cantidad de apariciones 
-- de cada caracter de la lista de caracteres
contarTodasApariciones :: String -> String -> [Int]
contarTodasApariciones [] [] = []
contarTodasApariciones [] (c:chars) = 0 : pasoRecursivo -- Si el string es vacio devuelve una lista con todos 0 y misma longitud que la lista minusculas
    where pasoRecursivo = contarTodasApariciones [] chars
contarTodasApariciones string (c:chars) = aparicionesM : aparicionesMs
    where
        -- Cuantas veces aparece el caracter de la lista de caracteres validos en el string
        aparicionesM = cantidadDeApariciones c string
        stringSinM = eliminarTodasApariciones c string
        -- Lista con la cantidad de apariciones de las otras letras de caracteres validos
        aparicionesMs = contarTodasApariciones stringSinM chars

-- Elimina del string todos los caracteres que no son minusculas
eliminarNoMinusculas :: String -> String -> String
eliminarNoMinusculas [] _ = []
eliminarNoMinusculas (x:xs) caracteresValidos
    | esMinuscula x = x : pasoRecursivo -- Deja la minuscula y continua con el resto del string
    | otherwise = pasoRecursivo -- Encontro no minuscula, la elimina y sigue buscando no minusculas
    where pasoRecursivo = eliminarNoMinusculas xs caracteresValidos

-- Elimina todas las apariciones del char
eliminarTodasApariciones :: Char -> String -> String
eliminarTodasApariciones _ [] = []
eliminarTodasApariciones char (c:chars)
    | char == c = pasoRecursivo
    | otherwise = c : pasoRecursivo
    where
        pasoRecursivo = eliminarTodasApariciones char chars

-- Cuenta cuantas veces aparece un caracter en un string
cantidadDeApariciones :: Char -> String -> Int
cantidadDeApariciones _ [] = 0
cantidadDeApariciones char (c:chars)
    | char == c = 1 + pasoRecursivo
    | otherwise = pasoRecursivo
    where
        pasoRecursivo = cantidadDeApariciones char chars

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente frase clave = (charcifrado, porcentaje)
    where
        charcifrado = desplazar caracter clave  -- Desplaza el caracter con la clave 
        fraseLimpia = eliminarNoMinusculas frase caracteresValidos -- Deja el string con solo catacteres validos
        caracter = caracterMasFrecuente fraseLimpia fraseLimpia -- Lo uso para achicar el codigo en la funcion principal
        porcentaje = dividir (cantidadDeApariciones caracter fraseLimpia) (length fraseLimpia) * 100 --Saca la frecuencia del caracter

-- Busca el caracter con mas aparaciones en una string
caracterMasFrecuente :: String -> String -> Char
caracterMasFrecuente _ [c1] = c1
caracterMasFrecuente fraseLimpia (c1:c2:chars) 
    -- Compara entre dos caracteres su cantidad de aparaciones y vuelve a aplicar la funcion con el mayor
    | cantC1 >= cantC2 =  pasoRecursivoC1 
    | otherwise = pasoRecursivoC2
    where
        cantC1 = cantidadDeApariciones c1 fraseLimpia
        cantC2 = cantidadDeApariciones c2 fraseLimpia
        pasoRecursivoC1 = caracterMasFrecuente fraseLimpia (c1:chars)
        pasoRecursivoC2 = caracterMasFrecuente fraseLimpia (c2:chars)

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado frase1 frase2 = esDescifradoAux frase1 frase2 25

esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux frase1 frase2 0 = frase1 == frase2
esDescifradoAux frase1 frase2 n = fraseLimpia1 == cifrar fraseLimpia2 n || esDescifradoAux frase1 frase2 (n-1)
    where fraseLimpia1 = eliminarNoMinusculas frase1 caracteresValidos
          fraseLimpia2 = eliminarNoMinusculas frase2 caracteresValidos

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados [x] 
    | sinMinusculas x = [(x,x)]
    | otherwise = []
todosLosDescifrados listFrases = todosLosDescifradosAux listFrases listFrases

--Creo una funcion auxiliar que duplica la lista que le paso y modifica la primera hasta quedarse con un elemento y vuelve a comenzar
todosLosDescifradosAux :: [String] ->[String]-> [(String,String)] 
todosLosDescifradosAux _ [] = []
todosLosDescifradosAux [f1] listFrases  
    | sinMinusculas f1 = (f1,f1): todosLosDescifradosAux (tail listFrases) (tail listFrases)
    | otherwise = todosLosDescifradosAux (tail listFrases) (tail listFrases) --Vuelve a utilizar la funcion sacando el primer elemento de la lista (listFrases)
todosLosDescifradosAux (f1:f2:fs) listFrases --Busca las palabras cifradas con el primer elemento
    | esDescifrado f1 f2 = (f1,f2):(f2,f1):todosLosDescifradosAux (f1:fs) listFrases --Si estan cifradas crea las dos tuplas y comienza de nuevo sin el segundo elemento
    | otherwise = todosLosDescifradosAux (f1:fs) listFrases --Si no lo estan solamente elimina el segundo elemento y comienza de nuevo

sinMinusculas :: String -> Bool
sinMinusculas [] = True
sinMinusculas (char:chars) 
    | not(esMinuscula char) = sinMinusculas chars
    | otherwise = False

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave string longitud = repetirStringLongitudN string longitud 0

-- Repite el string hasta la longitud n
repetirStringLongitudN :: String -> Int -> Int -> String
repetirStringLongitudN [] _ _ = []
repetirStringLongitudN string longitud indice
    -- (Longitud - 1) arranca desde el 0 al igual que el indice
    | indice <= (longitud - 1) = nesimoIndice string indiceEnRango : pasoRecursivo
    | otherwise = []
    -- Indice en rango siempre esta entre 0<= i < |string|
    where   indiceEnRango = mod indice strLen
            strLen = length string
            pasoRecursivo = repetirStringLongitudN string longitud (indice + 1)



nesimoIndice :: String -> Int -> Char
nesimoIndice string iDeseado = nesimoIndiceAux string iDeseado 0 

nesimoIndiceAux :: String -> Int -> Int -> Char
nesimoIndiceAux (x:xs) iDeseado iActual
    | iActual == iDeseado = x
    | otherwise = pasoRecursivo
    where pasoRecursivo = nesimoIndiceAux xs iDeseado (iActual + 1)

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere string clave = cifrarVigenereAux string claveExpandida
    where claveExpandida = obtenerClaveNaturalesExpandida string clave

cifrarVigenereAux :: String -> [Int] -> String
cifrarVigenereAux [] [] = []
cifrarVigenereAux (x:xs) (c:cs) = desplazar x c : pasoRecursivo
    where pasoRecursivo = cifrarVigenereAux xs cs

letrasANaturales :: String -> [Int]
letrasANaturales [] = []
letrasANaturales (l:letras) = letraANatural l : pasoRecursivo
    where pasoRecursivo = letrasANaturales letras

-- Expande la clave de caracteres y los convierte a una lista de claves naturales
obtenerClaveNaturalesExpandida :: String -> String -> [Int]
obtenerClaveNaturalesExpandida string clave = listaDeClavesNaturales
    where listaDeClavesNaturales = letrasANaturales claveExpandida
          claveExpandida = expandirClave clave strLen
          strLen = length string

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere string clave = descifrarVigenereAux string claveExpandida
    where claveExpandida = obtenerClaveNaturalesExpandida string clave

descifrarVigenereAux :: String -> [Int] -> String
descifrarVigenereAux [] [] = []
descifrarVigenereAux (x:xs) (c:cs) = desplazar x (-c) : pasoRecursivo
    where pasoRecursivo = descifrarVigenereAux xs cs

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ [clave] = clave
peorCifrado string (c1:c2:cs)
    | distanciaC1 <= distanciaC2 = pasoRecursivoC1
    | otherwise = pasoRecursivoC2
    where distanciaC1 = distanciaStringStringCifrado string c1
          distanciaC2 = distanciaStringStringCifrado string c2
          pasoRecursivoC1 = peorCifrado string (c1:cs)
          pasoRecursivoC2 = peorCifrado string (c2:cs)

naturalesDelCifrado :: String -> String -> [Int]
naturalesDelCifrado string clave = letrasANaturales claveCifrada
    where claveCifrada = cifrarVigenere string clave

distanciaEntreStrings :: String -> String -> Int
distanciaEntreStrings [] [] = 0
distanciaEntreStrings (s1:string1) (s2:string2) = absoluto (letraANatural s1 - letraANatural s2) + pasoRecursivo
    where pasoRecursivo = distanciaEntreStrings string1 string2

distanciaStringStringCifrado :: String -> String -> Int
distanciaStringStringCifrado string clave = distanciaEntreStrings string stringCifrado
    where stringCifrado = cifrarVigenere string clave

sumatoriaNaturales :: [Int] -> Int
sumatoriaNaturales [n] = n
sumatoriaNaturales (n:ns) = n + pasoRecursivo
    where pasoRecursivo = sumatoriaNaturales ns

absoluto :: Int -> Int
absoluto n
    | n < 0 = -n
    | otherwise = n

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (s:strings) claves cifradoDeseado = tuplaCombinacionesValidas1 ++ pasoRecursivo
    where combinacionS = listaCombinacionesStringClaves s claves
          tuplaCombinacionesValidas1 = tuplaCifradosDeseados combinacionS cifradoDeseado
          pasoRecursivo = combinacionesVigenere strings claves cifradoDeseado

listaCombinacionesStringClaves :: String -> [String] -> [(String, String)]
listaCombinacionesStringClaves _ [] = []
listaCombinacionesStringClaves string (c:claves) = (string, c) : pasoRecursivo
    where pasoRecursivo = listaCombinacionesStringClaves string claves

tuplaCifradosDeseados :: [(String,String)] -> String -> [(String, String)]
tuplaCifradosDeseados [] _ = []
tuplaCifradosDeseados ((string1,clave1):combinaciones) cifradoDeseado
    | s1Cifrado == cifradoDeseado = (string1,clave1) : pasoRecursivo
    | otherwise = pasoRecursivo
    where s1Cifrado = cifrarVigenere string1 clave1
          pasoRecursivo = tuplaCifradosDeseados combinaciones cifradoDeseado