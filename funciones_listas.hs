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


-- retorna el último elemento
last' :: [a] -> a 
last' [] = error "Lista vacía"
last' xs = xs !! ( length' xs - 1 )


-- Si la lista está vacía retorna verdadero, sino, Falso.
null' :: [a] -> Bool
null' xs = if (length' xs) == 0 then True else False


-- Retorna una cantidad n de elementos de la lista
take' :: Int -> [a] -> [a]
take' _ [] = []
take' a xs = let indice = a -1 in [ xs !! y | y <- [0..indice] ] 


-- Retorna la lista excepto el primer elemento
init' :: [a] -> [a]
init' xs = take' ( length' xs - 1) xs 


-- Invierte la lista
reverse' :: [a] -> [a]
reverse' xs = let indice = length' xs - 1 in [xs !! y | y <- [indice,(indice-1)..0]]


-- Elimina n elementos de una lista desde los primeros a los últimos
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' a xs = let indice = length' xs - 1 in [xs !! y | y <- [0,1..indice], y >= a]


-- Retorna el menor valor de una lista
minimum' :: Ord a => [a] -> a
minimum' [] = error "Lista vacía"
minimum' (x:xs) 
 | xs == [] = x
 | [x] < xs = minimum' (x:(drop' 1 xs)) 
 | otherwise = minimum' (xs)


-- Retorna el máximo valor de una lista
maximum' :: Ord a => [a] -> a
maximum' [] = error "Lista vacía"
maximum' (x:xs) 
 | xs == [] = x
 | [x] > xs = maximum' (x:(drop' 1 xs)) 
 | otherwise = maximum' (xs)


-- Suma todos los elementos de una lista
sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x:xs) = x + (sum' xs)  


-- Multiplica todos los elementos de una lista
product' :: Num p => [p] -> p
product' [] = 1
product' (x:xs) = x * (product' xs)  


-- Retorna un Booleano si el elemento pertenece a lista
elem' :: Eq a => a -> [a] -> Bool
elem' a xs = if null' [ y | y <- xs, y == a] then False else True


-- Repite n veces un elemento en forma de lista
repeat' :: Enum a => a -> [a]
repeat' x = [x,x..x]


-- Replica n veces una lista
replicate' :: (Num t, Enum t) => t -> a -> [a]
replicate' a x = [x | _ <- [1..a] ]

-- Elimina solo el primer elemento de una lista que corresponda a x
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' a (x:xs) = if a == x then xs else x:(delete' a xs)
-- delete' a (x:xs) = if a /= x then x:(delete' a xs) else xs

-- Aplica una función a una lista
map' :: (t -> a) -> [t] -> [a]
map' f lst = [ f x | x <- lst ]

-- Sobre los elementos respectivos de dos listas aplica una función
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = ( f x y ):(zipWith' f xs ys )

-- De una función gira el orden de los argumentos
flip' :: (a -> b -> c) -> b -> a -> c
flip' f a b = f b a 

-- Filtra los elementos de una lista que cumplen determinada condición de verdad (Bool)
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = [] 
filter' f lst = [ x | x <- lst, f x ] 

-- Retorna los elementos de la lista hasta que una condición deje de ser verdadera
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (x:xs) = if f x then x:takeWhile' f xs else []


-- Aplica una función transversalmente a una lista de izquierda a derecha tomando un valor inicial.
foldl' :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl' f a [x] = f a x
foldl' f a (x:xs) = foldl' f (f a x) xs


-- Aplica una función transversalmente a una lista de derecha a izquierda tomando un valor inicial.
foldr' :: (a -> t -> t) -> t -> [a] -> t
foldr' f a [x] = f x a
foldr' f a lst = foldr' f (f (last lst) a) (init lst)


-- Aplica una función transversalmente a una lista de izquierda a derecha tomando el primer valor de la lista
foldl1' :: (t2 -> t2 -> t2) -> [t2] -> t2
foldl1' f (x:xs) = foldl' f x xs

-- Aplica una función transversalmente a una lista de derecha a izquierda tomando el primer valor de la lista
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f lst  = foldr' f (last lst) (init lst)

-- Aplica una función transversalmente a una lista de derecha a izquierda mostrando todos los estados intermedios
scanl' f a [] = [a]
scanl' f a (x:xs) = let operacion = f a x in a : scanl' f operacion xs

-- Aplica una función transversalmente a una lista de derecha a izquierda mostrando todos los estados intermedios
rigth f a [] = [a]
rigth f a lst = let operacion = f (last lst) a in a : rigth f operacion (init lst)
scanr' = \f a lst -> reverse (rigth f a lst) 

-- Retorna la lista sin elementos duplicados
nub' [] = []
nub' (x:xs)
 | elem x xs = nub' xs
 | otherwise = x : nub' xs

-- Intercala un elemento a lo largo de una lista
intersperse' :: a -> [a] -> [a]
intersperse' a [] = []
intersperse' a (x:xs) = x:a:intersperse' a xs

-- Intercala una lista en un conjunto de listas
intercalate' xs [x] = x
intercalate' xs (y:ys) = y ++ xs ++ intercalate' xs ys 

-- De una listas de listas junta los elementos de indices respectivos en una nueva lista de listas
trans lst a
     | a == largo = [[]]
     | otherwise = [ x !! a | x <- lst, length x > a] : trans lst (a+1)
     where largo = length lst
transpose' lst = filter (/=[]) $ trans lst 0 

