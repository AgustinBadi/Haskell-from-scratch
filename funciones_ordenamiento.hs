-- Algoritmos de ordenamiento

-- Intercambia el primer y el último de una lista
swapBasico :: [a] -> [a]
swapBasico lst = last lst : tail (init lst) ++ [head lst]


-- Intercambia elementos de una lista según su posición
swapCoordenadas :: Int -> Int -> [a] -> [a]
swapCoordenadas x y lst = [if a == x then lst !! y else (if a == y then lst !! x else lst !! a) | a <- [0,1..(length lst - 1) ]]






