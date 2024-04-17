fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n<0 = error "Escriba numero mayor o igual que 0"
            | otherwise = fibonacci(n-1) + fibonacci(n-2)
-----------------------------------
parteEntera :: Float -> Int
parteEntera x   | x<1 && x>=0 = 0
                | x>(-1) && x<0 = -1
                | x>=1 = 1+parteEntera (x-1) 
                | x<0 = (-1) +parteEntera (x + 1)
-----------------------------------
esDivisible :: Int -> Int -> Bool
esDivisible x y | x-y == 0 = True
                | x-y < 0 = False
                |otherwise = esDivisible (x-y) y 
-----------------------------------
sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares x = sumaImpares (x-1) + (2*x-1)
-----------------------------------
medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact x | x>1 = x * medioFact(x-2)
            |otherwise = error "Elija numero positvo"
-----------------------------------
sumaDigitos :: Int -> Int
sumaDigitos n   | n<10 = n
                | otherwise = (mod n 10) + sumaDigitos (div n 10)
-----------------------------------
todosDigitosIguales :: Int -> Bool
todosDigitosIguales n   | n<10 = True
                        | n<100 = mod n 10 == div n 10
                        | otherwise = todosDigitosIguales (mod n 100) && todosDigitosIguales (div n 10)
------------------------------------
cantDigitos :: Int -> Int
cantDigitos n   | n<10 = 1
                | otherwise = 1 + cantDigitos (div n 10)
iesimoDigito :: Int -> Int -> Int
iesimoDigito n i    | cantDigitos n == i = sacarUltimoDigito
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
an :: Int -> Float
an 1 = 2
an n = 2 + 1/an (n-1)
raizDe2Aprox :: Int -> Float
raizDe2Aprox 1 = 1
raizDe2Aprox n = an (n) - 1
----------------------------------
g :: Int -> Int -> Int
g i 1 = i
g i m = i^m + g i (m-1)
f :: Int -> Int -> Int
f 1 m = g 1 m
f n m = f (n-1) m + g n m
----------------------------------
auxSumaPotencias :: Integer -> Integer -> Integer
auxSumaPotencias q 0 = 1
auxSumaPotencias q n = q^n + auxSumaPotencias q (n-1)
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q 0 0 = 1 
sumaPotencias q n m = auxSumaPotencias q n * auxSumaPotencias q m
----------------------------------
auxSumaRacionales :: Integer -> Integer -> Float
auxSumaRacionales p 0 = 0
auxSumaRacionales p q = fromIntegral p / fromIntegral q + auxSumaRacionales p (q-1)
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 0 q = 0
sumaRacionales p q = auxSumaRacionales p q + sumaRacionales (p-1) q 
----------------------------------
menorDivisorHasta :: Integer -> Integer -> Integer
menorDivisorHasta n m   | mod n m == 0 = m 
                        | otherwise = menorDivisorHasta n (m+1)
menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = menorDivisorHasta n 2 
----------------------------------
esPrimo :: Integer -> Bool
esPrimo n = n == menorDivisor n
----------------------------------
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (rem a b )
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = mcd n m == 1
----------------------------------
siguientePrimo :: Integer -> Integer
siguientePrimo n    | esPrimo (n+1) = n+1
                    | otherwise = siguientePrimo (n+1)
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo 1 = 2
nEsimoPrimo n = siguientePrimo (nEsimoPrimo (n-1))    
----------------------------------
