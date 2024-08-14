doubleMe :: Int -> Int
doubleMe x = x + x
--------------------------------
f n     | n == 1 = 8
        | n == 4 = 131
        | n == 16 = 16
        | otherwise = error "Elija un numero entre 1, 4 o 16"
--------------------------------
g :: Int -> Int
g n     | n == 8 = 16
        | n == 16 = 4
        | n == 131 = 1
        | otherwise = error "Elija un numero entre 8, 16 o 131"
--------------------------------
h :: Int -> Int
h n = f(g n)
--------------------------------
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z   |x >= y && x>= z = x
                |y >= x && y >= z = y
                |otherwise = z
--------------------------------
absoluto :: Int-> Int
absoluto x      | x>0 = x
                | otherwise = (-x)
digitoUnidades :: Int -> Int
digitoUnidades x = mod (absoluto x) 10
--------------------------------
digitoDecenas :: Int -> Int
digitoDecenas x = div(mod (absoluto x) 100) 10
--------------------------------
-- Usando pattern Maching
todoMenor :: (Float,Float) -> (Float,Float) -> Bool
todoMenor (a,b) (c,d) = a<c && b<d 
--------------------------------                     
mismomenor :: (Float,Float) -> (Float,Float) -> Bool
mismomenor t1 t2 = (fst t1) < (fst t2) && (snd t1) < (snd t2) 
-------------------------------- 
distanciaPuntos :: (Int,Int) -> (Int,Int) -> (Int,Int)
distanciaPuntos (a,b) (c,d) = (absoluto(c-a),absoluto(d-b))
--------------------------------
sumaTerna :: (Float,Float,Float) -> Float
sumaTerna (x,y,z) = x + y + z
--------------------------------
sumarSoloMultiplos :: (Int,Int,Int) -> Int -> Int
sumarSoloMultiplos (x,y,z) n    | mod (absoluto x) n == 0 && mod (absoluto y) n == 0 && mod (absoluto z) n == 0 = x + y + z
                                | mod (absoluto x) n == 0 && mod (absoluto y) n == 0 = x + y 
                                | mod (absoluto x) n == 0 && mod (absoluto z) n == 0 = x + z
                                | mod (absoluto z) n == 0 && mod (absoluto y) n == 0 = z + y  
                                | mod (absoluto x) n == 0 = x
                                | mod (absoluto y) n == 0 = y
                                | mod (absoluto z) n == 0 = z
                                | otherwise = 0
--------------------------------
posPrimerPar :: (Int,Int,Int) -> Int
posPrimerPar (x,y,z)    | mod (absoluto x) 2 == 0 = 1
                        | mod (absoluto y) 2 == 0 = 2
                        | mod (absoluto z) 2 == 0 = 3
                        | otherwise = 4
--------------------------------
crearPar :: a -> b  -> (a,b)
crearPar x y =(x,y)
--------------------------------
invertir :: (a,b)  -> (b,a)
invertir (x,y) =(y,x)
--------------------------------
ff :: Int -> Int
ff n    | n <= 7 = n^2
        | n > 7 = (2*n)-1
--------------------------------
gg :: Int -> Int
gg n    | mod (absoluto n) 2 == 0 = div n 2 
        | otherwise = (3*n)+1
--------------------------------
todosMenores :: (Int,Int,Int) -> Bool
todosMenores (x,y,z) = ff x > gg x && ff y > gg y && ff z > gg z 
--------------------------------
absf :: Float -> Float
absf x  | x>0 = x
        | otherwise = (-x)
--------------------------------
distanciaManhattan :: (Float,Float,Float) -> (Float,Float,Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) = absf(a-d)+absf(b-e)+absf(c-f)
--------------------------------
estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b = a^2+a*b*div(-a)b == 0
--------------------------------
sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = mod (absoluto x) 10 + mod (div (absoluto x) 10) 10
--------------------------------
comparar :: Int -> Int -> Int
comparar a b    | sumaUltimosDosDigitos(a) < sumaUltimosDosDigitos(b) = 1
                | sumaUltimosDosDigitos(a) > sumaUltimosDosDigitos(b) = (-1)
                | otherwise = 0