
-- Secuencia de números primos

esPrimo a = [ if mod a b /= 0 then False else True |  b <- [2,3..(a-1)] ] 
esBool [] = False
esBool (a:arr) = a || ( esBool arr )
primos = 1:[ x | x <- [2,3..], (esBool (esPrimo x)) == False ]



-- Secuencia infinita de números de fibonacci
fib :: Integral t => t -> t -> [t]
infinitosFibonacci :: [Integer]

fib a b = let suma = a + b in suma:(fib b suma)   
infinitosFibonacci = 1:(fib 0 1)
-- infFibs = 0 : 1 : zipWith (+) infFibs ( tail infFibs)

-- Implementación función mod
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


