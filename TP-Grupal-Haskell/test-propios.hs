import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    "Caracter espacio" ~: esMinuscula ' ' ~?= False,
    "Caracter minuscula" ~: esMinuscula 'd' ~?= True,
    "Caracter mayuscula" ~: esMinuscula 'M' ~?= False,
    "Letra ñ" ~: esMinuscula 'ñ' ~?= False,
    "Letra con tilde" ~: esMinuscula 'á' ~?= False,
    "Caracter especial" ~: esMinuscula '*' ~?= False,
    "Caracteres numericos" ~: esMinuscula '8' ~?= False
    ]

testsEjletraANatural = test [
    "Primer posicion" ~: letraANatural 'a' ~?= 0,
    "N-ecima posicion" ~: letraANatural 'b' ~?= 1,
    "Ultima posicion" ~: letraANatural 'z' ~?= 25,
    -- Casos extra
    "Natural de 'j'" ~: letraANatural 'j' ~?= 9,
    "Natural de 'y'" ~: letraANatural 'y' ~?= 24,
    "Natural de 's'" ~: letraANatural 's' ~?= 18
    ]

testsEjdesplazar = test [
    "Minuscula con desplazamiento -" ~: desplazar 'a' (-3) ~?= 'x',
    "Minuscula sin desplazamiento" ~: desplazar 'a' 0 ~?= 'a',
    "Minuscula con desplazamiento +" ~: desplazar 'j' 3 ~?= 'm',
    "No minuscula" ~: desplazar 'ñ' 5 ~?= 'ñ',
    "Minuscula sin desplazamiento fuera de rango" ~: desplazar 'a' (-26) ~?= 'a',
    "Minuscula con desplazamiento - de rango" ~: desplazar 'a' (-27) ~?= 'z',
    "Minuscula con desplazamiento + fuera de rango" ~: desplazar 'a' 28 ~?= 'c',
    -- Casos extra
    "No minuscula caracter espacio" ~: desplazar ' ' 83 ~?= ' '
    ]

testsEjcifrar = test [
    "String vacio" ~: cifrar "" 2 ~?= "",
    "String ninguna minuscula" ~: cifrar "BASTAAAA" 69 ~?= "BASTAAAA",
    "String todas minusculas sin desplazamiento" ~: cifrar "noestoycifrado" 0 ~?= "noestoycifrado",
    "String algunas minusculas sin desplazamiento" ~: cifrar "ábñcLd" 0 ~?= "ábñcLd",
    "String todas minusculas con desplazamiento" ~: cifrar "computacion" 3 ~?= "frpsxwdflrq",
    "String algunas minusculas con desplazamiento" ~: cifrar "estoy Cifrado" 5 ~?= "jxytd Cnkwfit",
    "String algunas minusculas con desplazamiento fuera de rango" ~: cifrar "fuera de rango" 27 ~?= "gvfsb ef sbohp",
    "String algunas minusculas sin desplazamiento fuera de rango" ~: cifrar "no estoy cifrado" 26 ~?= "no estoy cifrado"
    ]

testsEjdescifrar = test [
    "String vacio" ~: descifrar "" 5 ~?= "",
    "String ninguna minuscula" ~: descifrar "BASTAAAA" 69 ~?= "BASTAAAA",
    "String no vacio sin desplazamiento" ~: descifrar "hola como estas" 0 ~?= "hola como estas",
    "String no vacio con desplazamiento +" ~: descifrar "frpsxwdflrq" 3 ~?= "computacion",
    "String todas minusculas sin desplazamiento" ~: descifrar "noestoycifrado" 0 ~?= "noestoycifrado",
    "String algunas minusculas sin desplazamiento" ~: descifrar "ábñcLd" 0 ~?= "ábñcLd",
    "String algunas minusculas con desplazamiento" ~: descifrar "jxytd Cnkwfit" 5 ~?= "estoy Cifrado",
    "String algunas minusculas con desplazamiento fuera de rango" ~: descifrar "gvfsb ef sbohp" 27 ~?= "fuera de rango",
    "String algunas minusculas sin desplazamiento fuera de rango" ~: descifrar "no estoy cifrado" 26 ~?= "no estoy cifrado"
    ]

testsEjcifrarLista = test [
    "Ningun elemento" ~: cifrarLista [] ~?= [],
    "1 elemento todas minusculas" ~: cifrarLista ["sincifrar"] ~?= ["sincifrar"],
    "1 elemento algunas minusculas" ~: cifrarLista ["sin cifrar"] ~?= ["sin cifrar"],
    "3 elementos algunas minusculas y un string vacio" ~: cifrarLista ["Benja","Demian", "Hola", ""] ~?= ["Benja", "Dfnjbo", "Hqnc", ""],
    "3 elementos todas minusculas" ~: cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
    "1 elemento repetido algunas minusculas" ~: cifrarLista ["Repetido","labo","Repetido","bien"] ~?= ["Repetido", "mbcp", "Rgrgvkfq","elhq"],
    "2 elementos repetidos algunas minusculas" ~: cifrarLista ["Repetido","labo","bien","Repetido","bien","Repetido"] ~?= ["Repetido", "mbcp", "dkgp", "Rhshwlgr", "fmir", "Rjujynit"]
    ]

testsEjfrecuencia = test [
    "String vacio" ~: frecuencia "" ~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "String no vacio 1 sola letra minuscula" ~: frecuencia "aaaNOSOYMINUSCULA" ~?= [100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "String no vacio con dos letras repetidas igual veces" ~: expectlistProximity (frecuencia "llBLA BLAtt") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,50.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,50.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "String no vacio con dos letras repetidas distintas veces" ~: expectlistProximity (frecuencia "hhBLhABLAtt") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,(3/5)*100,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,(2/5)*100,0.0,0.0,0.0,0.0,0.0,0.0],
    "String no vacio ninguna minuscula" ~: frecuencia "FLDSMDFR" ~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "String no vacio con algunas minusculas" ~: expectlistProximity (frecuencia "tallEerñS") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    "String no vacio todas minusculas" ~: expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

testsEjcifradoMasFrecuente = test [
    "Una letra con desplazamiento +" ~: cifradoMasFrecuente "z" 2 ~?= ('b',100),
    "Una letra con desplazamiento -" ~: cifradoMasFrecuente "r" (-3) ~?= ('o',100),
    "Una letra sin desplazamiento" ~: cifradoMasFrecuente "a" 0 ~?= ('a',100),
    "Una letra con desplazamiento + fuera de rango" ~: cifradoMasFrecuente "h" 29 ~?= ('k',100),
    "Una letra con desplazamiento - fuera de rango" ~: cifradoMasFrecuente "h" (-50) ~?= ('j',100),
    "Empate dos letras con desplazamiento +" ~: expectAnyTuplaAprox (cifradoMasFrecuente "ja" 8) [('r',50.0),('i',50.0)],
    "Empate dos letras con desplazamiento -" ~: expectAnyTuplaAprox (cifradoMasFrecuente "hJSDJa" (-27)) [('g',50.0),('z',50.0)],
    "Empate tres letras con desplazamiento +" ~: expectAnyTuplaAprox (cifradoMasFrecuente "nJJos" 10) [('x',(1/3)*100),('c',(1/3)*100),('y',(1/3)*100)],
    "Empate tres letras con desplazamiento -" ~: expectAnyTuplaAprox (cifradoMasFrecuente "nnJJooss" (-20)) [('t',(2/6)*100),('y',(2/6)*100),('u',(2/6)*100)],
    "String una minuscula con desplazamiento +" ~: cifradoMasFrecuente "LAñ PAdP" 4 ~?= ('h',100),
    "String una minuscula con desplazamiento -" ~: cifradoMasFrecuente "LAñPcPñ" (-2) ~?= ('a',100),
    "String una minuscula sin desplazamiento" ~: cifradoMasFrecuente "PRuEBA" 0 ~?= ('u',100),
    "String todas minusculas con desplazamiento +" ~: expectAnyTuplaAprox (cifradoMasFrecuente "avellaneda" 6) [('g',30.0)],
    "String todas minusculas con desplazamiento -" ~: expectAnyTuplaAprox (cifradoMasFrecuente "persona" (-1)) [('o',(1/7)*100),('d',(1/7)*100),('q',(1/7)*100),('r',(1/7)*100),('n',(1/7)*100),('m',(1/7)*100),('z',(1/7)*100)],
    "String todas minusculas sin desplazamiento" ~: expectAnyTuplaAprox (cifradoMasFrecuente "antartida" 0)  [('a',33.333333)],
    "String completo con desplazamiento +" ~: expectAnyTuplaAprox (cifradoMasFrecuente "comPuTaCion" 1) [('p',25.0)],
    "String completo con desplazamiento -" ~: expectAnyTuplaAprox (cifradoMasFrecuente "TermoDinamica" (-3)) [('j',(2/11)*100),('x',(2/11)*100),('f',(2/11)*100)],
    "String completo sin desplazamiento" ~: expectAnyTuplaAprox (cifradoMasFrecuente "antartida" 0) [('a',33.333333)]
    ]

testsEjesDescifrado = test [
    "Strings vacios" ~: esDescifrado "" "" ~?= True,
    "String con vacio" ~: esDescifrado "Excelencia" "" ~?= False,
    "Vacio con String" ~: esDescifrado "" "Fracaso" ~?= False,
    "Todas no minusculas" ~: esDescifrado "GUACAMOLE" "GUACAMOLE" ~?= True,
    "Strings iguales sin cifrar, n=0" ~: esDescifrado "igualdad" "igualdad" ~?= True,
    "Strings distintos" ~: esDescifrado "taller" "compu" ~?= False,
    "Strings cifrados, n=2" ~: esDescifrado "ciencia" "ekgpekc" ~?= True,
    "Strings mezclado cifrados, n=-1" ~: esDescifrado "TermInaTor" "TdqlImzTnq" ~?= True
    ]

testsEjtodosLosDescifrados = test [
    "Lista vacia" ~: todosLosDescifrados [] ~?= [],     
    "String vacio" ~: todosLosDescifrados [""] ~?= [("","")],
    "Un char noMinuscula" ~: todosLosDescifrados ["A"] ~?= [("A","A")],
    "Un elemento" ~: todosLosDescifrados ["Esquina"] ~?= [],
    "Elementos con minusculas y noMinuscula" ~: expectPermutacion (todosLosDescifrados ["A","a","b"]) [("a","b"),("b","a"),("A","A")],
    "Dos elementos cifrados" ~: expectPermutacion (todosLosDescifrados ["compu", "frpsx"]) [("compu","frpsx"),("frpsx","compu")],
    "Dos elementos no cifrados" ~: todosLosDescifrados ["arbol","pelota"] ~?= [],
    "Varios elementos pero 1 par" ~: expectPermutacion (todosLosDescifrados ["botella","mouse","suayv","uhmxeet"]) [("botella","uhmxeet"),("uhmxeet","botella")],
    "Varios elementos con mas pares" ~: expectPermutacion (todosLosDescifrados ["xqdtrqbb","armario","izuizqw","handball","dupdulr"]) [("handball","xqdtrqbb"),("xqdtrqbb","handball"),("armario","izuizqw"),("izuizqw","armario"),("armario","dupdulr"),("dupdulr","armario"),("izuizqw","dupdulr"),("dupdulr","izuizqw")],
    "Dos elementos cifrados con algunas minusculas" ~: expectPermutacion (todosLosDescifrados ["Ej nuMero 2","Em qxMhur 2","ej numero 2"]) [("Ej nuMero 2","Em qxMhur 2"),("Em qxMhur 2","Ej nuMero 2")]
    ]

testsEjexpandirClave = test [
    "|clave| = n = 1" ~: expandirClave "b" 1 ~?= "b",
    "|clave| > n " ~: expandirClave "holamundo" 5 ~?= "holam",
    "|clave| = n > 1" ~: expandirClave "discord" 7 ~?= "discord",
    "|clave| < n" ~: expandirClave "compu" 8 ~?= "compucom",
    -- Casos extra
    "|clave| < n" ~: expandirClave "blabla" 10 ~?= "blablablab",
    "|clave| > n" ~: expandirClave "arquitectura" 2 ~?= "ar"
    ]

testsEjcifrarVigenere = test [
    "|string| = 0" ~: cifrarVigenere "" "blabalb" ~?= "",
    "|string| < |clave| y string todas minusculas" ~: cifrarVigenere "haskell" "recursion" ~?= "yeuevdt",
    "|string| < |clave| y string algunas minusculas" ~: cifrarVigenere "lol" "mordekaiser" ~?= "xcc",
    "|string| > |clave| y string algunas minusculas" ~: cifrarVigenere "hola computacion" "hola" ~?= "ocwa qzmwieajwzn",
    "|string| > |clave| y string todas minusculas" ~: cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    "|string| = |clave| y string algunas minusculas" ~: cifrarVigenere "deMIan" "tienda" ~?= "wmMIdn",
    -- Casos extra
    "Clave todas 'a'" ~: cifrarVigenere "no estoy cifrado" "aaaa" ~?= "no estoy cifrado",
    "Clave todas iguales" ~: cifrarVigenere "Soy un test" "jjjjj" ~?= "Sxh dw cnbc",
    "Algunas minusculas con mayusculas y no minusculas" ~: cifrarVigenere "Mate con Jugoñ" "re" ~?= "Meki gfr Jlkfñ"
    ]

testsEjdescifrarVigenere = test [
    "|string| = 0" ~: descifrarVigenere "" "blabalb" ~?= "",
    "|string| < |clave| y string todas minusculas" ~: descifrarVigenere "yeuevdt" "recursion" ~?= "haskell",
    "|string| < |clave| y string algunas minusculas" ~: descifrarVigenere "xcc" "mordekaiser" ~?= "lol",
    "|string| > |clave| y string algunas minusculas" ~: descifrarVigenere "ocwa qzmwieajwzn" "hola" ~?= "hola computacion",
    "|string| > |clave| y string todas minusculas" ~: descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    "|string| = |clave| y string algunas minusculas" ~: descifrarVigenere "wmMIdn" "tienda" ~?= "deMIan",
    -- Casos extra
    "Clave todas 'a'" ~: descifrarVigenere "no estoy cifrado" "aaaa" ~?= "no estoy cifrado",
    "Clave todas iguales" ~: descifrarVigenere "Sxh dw cnbc" "jjjjj" ~?= "Soy un test",
    "Algunas minusculas con mayusculas y no minusculas" ~: descifrarVigenere "Meki gfr Jlkfñ" "re" ~?= "Mate con Jugoñ"
    ]

testsEjpeorCifrado = test [
    "Cifrar vacio" ~: expectAny (peorCifrado "" ["bla","compu","tacion"]) ["bla","compu","tacion"],
    "Una clave" ~: peorCifrado "computadora" ["epas"] ~?= "epas",
    "Tres claves distintas" ~: peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
    "Una clave de solo 'a'" ~: peorCifrado "escuela" ["aaa", "zlw", "esperanza"] ~?= "aaa",
    "Dos claves y una solo 'z'" ~: peorCifrado "termo" ["zzz","algoritmo","hvjnm","lol"] ~?= "zzz",
    "Dos claves iguales" ~: peorCifrado "minecra" ["hvjnm","hvjnm"] ~?= "hvjnm",
    "Dos claves iguales y una peor" ~: peorCifrado "termo" ["hvjnm","hvjnm","lol"] ~?= "lol",
    "Dos claves empatan" ~: expectAny (peorCifrado "ventana" ["b","aaaaaah"]) ["b","aaaaaah"],
    "Dos claves empatan y una peor" ~: peorCifrado "ventana" ["b","aaaaaah","aa"] ~?= "aa",
    "Dos claves empatan y una mejor" ~: expectAny (peorCifrado "ventana" ["b","aaaaaah","v"]) ["b","aaaaaah"]
    ]

testsEjcombinacionesVigenere = test [
    "Ningun string" ~: combinacionesVigenere [] [] "claveultrasecreta" ~?= [],
    "1 string ninguna combinacion" ~: combinacionesVigenere ["sale lolcito"] ["no"] "Hola que onda" ~?= [],
    "1 string 1 combinacion" ~: combinacionesVigenere ["sale lolcito"] ["si"] "kidm tgtuqlw" ~?= [("sale lolcito", "si")],
    "2 strings 1 combinacion" ~: combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")],
    "2 strings ninguna combinacion" ~: combinacionesVigenere ["Buenas", "Adios"] ["ala", "bc"] "sincombinacion" ~?= [],
    "+2 strings +1 combinacion" ~: expectPermutacion (combinacionesVigenere ["hola", "soy main teemo", "hvio"] ["a", "b", "bueno"] "ipmb") [("hola","b"),("hvio","bueno")],
    "+2 strings ninguna combinacion" ~: expectPermutacion (combinacionesVigenere ["hola", "soy main teemo", "hvio"] ["a", "b", "bueno"] "clave") [],
    "Strings con letras mayusculas y combinacion" ~: expectPermutacion (combinacionesVigenere ["HOLA", "abc"] ["gato","perro"] "HOLA") [("HOLA","gato"),("HOLA","perro")],
    "Strings con letras mayusculas sin combinacion" ~: combinacionesVigenere ["HOLA", "PRECAUCION"] ["a","perro"] "CUIDADO" ~?= []
    ]

-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)