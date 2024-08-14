{-Ejercicio 1
problema aproboMasDeNMaterias (registro : seq(seq(Char) x seq(Z)), alumno: seq(Char) n: Z) : Bool
requiere: No hay nombres de alumnos repetidos en registro
requiere: Las notas de registro son todas iguales o mayores a 0 y menores o iguales a 10
requiere: n>0
requiere: el alumno se encuentra en el registro
asegura: res=true <-> el alumno tiene mas de n notas de finales mayores o iguales a 4 en el registro-}
                        --nombre x nota   --alumnos --n 
aproboMasDeNMaterias :: [([Char],[Int])] -> [Char] -> Int -> Bool
aproboMasDeNMaterias ((nombre,notas):registros) alumno n| alumno==nombre = cantidadAprobadas notas > n
                                                        | otherwise = aproboMasDeNMaterias registros alumno n

cantidadAprobadas :: [Int] -> Int
cantidadAprobadas [] = 0
cantidadAprobadas (x:xs) | x>=4 = 1 + cantidadAprobadas xs
    |otherwise = cantidadAprobadas xs

{-Ejercicio 2
problema buenosAlumnos (registro: seq(seq(Char) x seq(Z))) : seq(seq [Char])
requiere: No hay nombres de alumnos repetidos en registro
requiere: Las notas de registro son todas iguales o mayores a cero y menos o iguales a 10
asegura: res en la lista de los nombres de los alumnos que estan en registro cuyo promedio de notas es mayor o igual a 8 y no tiene aplazos(notas menores que 4)-}

buenosAlumnos :: [([Char],[Int])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos ((alumno,nota):registro)  | promedio nota >= 8 && tieneTodoAprobado nota = alumno:buenosAlumnos registro
                                        | otherwise = buenosAlumnos registro
promedio :: [Int] -> Float
promedio notas = division (suma notas) (longitud notas)
tieneTodoAprobado :: [Int] -> Bool 
tieneTodoAprobado [nota] = nota >= 4
tieneTodoAprobado (n1:ns) = n1 >=4 && tieneTodoAprobado ns
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs
longitud :: [Int] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b

{-Ejercicio 3
problema mejorPromedio (registro: seq(seq(Char) x seq(Z))) : seq(Char)
requiere: No hay nombres de alumnos repetidos en registro
requiere: Las notas de registro son todas iguales o mayores a cero y menos o iguales a 10
requiere: |registro| > 0
asegura: res es el nombre del alumno cuyo promedio de notas es el mas alto: si hay mas de un alumno con el mismo promeido de notas, devuelve el nombre que aparece primero en el registro -}

mejorPromedio :: [([Char],[Int])] -> [Char]
mejorPromedio [(alumno,_)] = alumno
mejorPromedio ((al1,n1):(al2,n2):registro) | promedio n1 >= promedio n2 = mejorPromedio ((al1,n1):registro)
    | otherwise = mejorPromedio ((al2,n2):registro)

{- Ejercicio 4 
problema seGraduoConHonores (registro: seq(seq(Char) x seq(Z))), cantidadDeMateriasDeLaCarrera: Z, alumno: seq[Char]): Bool
requiere: No hay nombres de alumnos repetidos en registro
requiere: Las notas de registro son todas iguales o mayores a cero y menos o iguales a 10
requiere: cantidadDeMateriasDeLaCarrera > 0
requiere: el alumno se encuentra en el registro
requiere: |buenosAlumnos(registro)| > 0
asegura: res <-> true si aproboMasDeNMaterias(registro,alumno,cantidadDeMateriasDeLaCarrera - 1) = True y alumno pertenece al conjunto de buenosAlumnos(registro) y el promedio de notas de finales de alumno esta a menos(estrictamente) de 1 punto del mejorPromedio(registro)-}

seGraduoConHonores :: [([Char],[Int])] -> Int -> [Char] -> Bool
seGraduoConHonores registro cantMaterias alumno = aproboMaterias && esBuenAlumno && buenPromedio
    where 
        aproboMaterias = aproboMasDeNMaterias registro alumno (cantMaterias - 1)
        esBuenAlumno = pertenece alumno (buenosAlumnos registro)
        buenPromedio = promedio (obtenerNotas alumno registro) + 1 > promedio (obtenerNotas (mejorPromedio registro) registro) 

obtenerNotas :: [Char] -> [([Char],[Int])] -> [Int]
obtenerNotas alumno ((nombre,nota):xs) | alumno==nombre = nota
    | otherwise = obtenerNotas alumno xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs) | t==x = True
    |otherwise = pertenece t xs