
-- Secuencia de números primos
-- primos = 1 : [ a | a <- [1,2..], b <- [1,2..(a-1)], mod a b == 0] 

-- Secuencia de números de fibonacci

fib :: Integral t => t -> t -> [t]
infinteFibonacci :: [Integer]

fib a b = let suma = a + b in suma:(fib b suma)   
infinteFibonacci = 1:(fib 0 1)

-- Implementación mod

mod' :: Int -> Int -> Int
mod' a b
 | diferencia == 0 = 0
 | diferencia == b = 0
 | diferencia < b = diferencia
 | diferencia > b = mod' diferencia b
 where diferencia = a - b 

 -- Suma los números impares dentro de una lista

sumarImpares [] = 0
sumarImpares (a:arr)
 | a `mod` 2 == 0 = sumarImpares arr
 | a `mod` 2 == 1 = a + (sumarImpares arr )

-- Devuelve los valores absolutos de una lista

abs' [] = []
abs' (a:arr)
 | a < 0 = a * (-1) : ( abs' arr)
 | a > 0 = a : (abs' arr)

