-- Implementación funciones de listas 

-- retorna el primer elemento
head' :: [a] -> a
head' xs = xs !! 0
-- head' x:xs = x

-- retorna la lista sin el primer elemento
tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

-- retorna el largo de la lista
length' :: [a] -> Int
length' [] = 0
length' xs = sum [1 | _ <- xs] 

-- retorna el último elemento (Corregir)
last' :: [a] -> a 
-- last' [] = ?
last' xs = xs !! ( length' xs - 1 )


-- Si la lista está vacía retorna verdadero, sino, Falso.
null' :: [a] -> Bool
null' xs = if (length' xs) == 0 then True else False

-- Retorna una cantidad n de elementos de la lista
take' :: Int -> [a] -> [a]
take' a xs = let indice = a -1 in [ xs !! y | y <- [0..indice] ] 
-- Tira una excepción con indices muy largos, arreglar.

-- Retorna la lista excepto el primer elemento
init' :: [a] -> [a]
init' xs = take' ( length' xs - 1) xs 

-- Invierte la lista
reverse' :: [a] -> [a]
reverse' xs = let indice = length' xs - 1 in [xs !! y | y <- [indice,(indice-1)..0]]


drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' a xs = let indice = length' xs - 1 in [xs !! y | y <- [0,1..indice], y >= a]

minimum' :: Ord a => [a] -> a
minimum' (x:xs) 
 | xs == [] = x
 | [x] < xs = minimum' (x:(drop' 1 xs)) 
 | otherwise = minimum' (xs)

maximum' :: Ord a => [a] -> a
maximum' (x:xs) 
 | xs == [] = x
 | [x] > xs = maximum' (x:(drop' 1 xs)) 
 | otherwise = maximum' (xs)

sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x:xs) = x + (sum' xs)  

product' :: Num p => [p] -> p
product' [] = 1
product' (x:xs) = x * (product' xs)  


elem' :: Eq a => a -> [a] -> Bool
elem' a xs = if null' [ y | y <- xs, y == a] then False else True

repeat' :: Enum a => a -> [a]
repeat' x = [x,x..x]

replicate' :: (Num t, Enum t) => t -> a -> [a]
replicate' a x = [x | _ <- [1..a] ]




