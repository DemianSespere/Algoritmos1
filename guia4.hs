sumaDigitos :: Int -> Int
sumaDigitos n | n<10 = n
              | otherwise = (mod n 10) + sumaDigitos (div n 10)
-----------------------------------
todosDigitosIguales :: Int -> Bool
todosDigitosIguales n | n<10 = True
                      | n<100 = mod n 10 == div n 10
                      | otherwise = todosDigitosIguales (mod n 100) && todosDigitosIguales (div n 10)
------------------------------------
cantDigitos :: Int -> Int
cantDigitos n | n<10 = 1
              | otherwise = 1 + cantDigitos (div n 10)
iesimoDigito :: Int -> Int -> Int
iesimoDigito n i | cantDigitos n == i = sacarUltimoDigito
                 | otherwise = iesimoDigito (div n 10) i 
                 where sacarUltimoDigito = mod n 10
-----------------------------------
inverso :: Int -> Int
inverso n = read (reverse(show n))
esCapicua :: Int -> Bool
esCapicua n = n == inverso n
--Show pasa de Integer a string
--Reverse da vuelta el string
--Read pasa de string a Integer (o a Float si lo designo asi)
----------------------------------
f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1 (n-1)
f2 :: Int -> Float -> Float
f2 1 q = q
f2 n q = q^n + f2 (n-1) q 
f3 :: Int -> Float -> Float
f3 n q = f2 (n*2) q
f4 :: Int -> Float -> Float 
f4 n q = f2 (2*n) q - f2 (n-1) q
----------------------------------
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
eAprox :: Int -> Float
eAprox 0 = 1
eAprox 1 = 2
eAprox n = 1/ fromIntegral (factorial n)+ eAprox(n-1) 
e :: Float
e = eAprox 10
----------------------------------
g :: Int -> Int -> Int
g i 1 = i
g i m = i^m + g i (m-1)
f :: Int -> Int -> Int
f 1 m = g 1 m
f n m = f (n-1) m + g n m
----------------------------------
menorDivisorHasta :: Integer -> Integer -> Integer
menorDivisorHasta n m | mod n m == 0 = m 
                      | otherwise = menorDivisorHasta n (m+1)
menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = menorDivisorHasta n 2 