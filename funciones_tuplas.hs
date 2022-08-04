-- Funciones tuplas

-- De una tupla retorna el primer elemento
fst' :: (a, b) -> a
fst' (a, _) = a

-- De una tupla retorna el segundo elemento
snd' :: (a, b) -> b 
snd' (_, b) = b 

-- Retorna una lista de tuplas a partir de dos listas
zip' :: [a] -> [b] -> [(a, b)]
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys) 