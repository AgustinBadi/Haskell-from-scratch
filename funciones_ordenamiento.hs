-- Algoritmos de ordenamiento


-- Intercambia el primer y el último de una lista
swapBasico :: [a] -> [a]
swapBasico lst = last lst : tail (init lst) ++ [head lst]

-- Si la lista es ascendente retorna verdadero
esAsc [x] = True
esAsc (x:xs)
 | x < head xs = esAsc xs
 | otherwise = False

-- Intercambia elementos de una lista según su posición
swapCoordenadas :: [a] -> Int -> Int ->  [a]
swapCoordenadas lst x y = [if a == x then lst !! y else (if a == y then lst !! x else lst !! a) | a <- [0,1..(length lst - 1) ]]



-- Selection Sort
-- Explicación: https://www.youtube.com/watch?v=g-PGLbMth_g

-- Retorna de una lista el menor valor y su indice
selection :: (Ord a, Num b) => ([a], b, b) -> (a, b, b)
selection ((x:xs),a,b)
 | x <= cabeza && length cola == 0 = (x,siguiente,b)
 | x > cabeza && length cola == 0 = (cabeza,siguiente,siguiente)
 | x <= cabeza = selection ((x:cola),siguiente,b)
 | x > cabeza = selection ((xs),siguiente,siguiente)
 where cabeza = head xs
       cola = tail xs
       siguiente = a + 1

-- Hace 1 iteración del ordenamiento
minIndex :: (a, b, c) -> c
minIndex (_,_,c) = c

ssort :: Ord a => [a] -> [a]
ssort lst = 
      let minimo = selection (lst,0,0)
      in swapCoordenadas lst 0 (minIndex minimo)

-- Realiza todas las iteraciones y retorna la lista ordenada
selectionSort :: Ord a => [a] -> [a]
selectionSort [x] = [x]
selectionSort lst = let actual = ssort lst in (head actual):(selectionSort (tail actual)) 



-- Insertion Sort
-- Explicación: https://www.youtube.com/watch?v=JU767SDMDvA

isort a [] = [a]
isort a (x:xs)
 | a <= x = a:x:xs
 | otherwise = x:(isort a xs)

insertion (xs,[]) = xs
insertion (xs,ys)
 | last xs <= head ys = insertion ( xs ++ [head ys], tail ys)
 | last xs > head ys = insertion ((isort (head ys) xs), tail ys) 

insertionSort (x:xs) = insertion ([x],xs)



-- Bubble Sort
-- Explicación: https://www.youtube.com/watch?v=xli_FI7CuzA
 
bubble [x] = [x]
bubble (x:xs)
 | x <= head xs = x:bubble xs
 | x > head xs = head xs : ( bubble $  x : tail xs ) 

bubbleSort lst = if esAsc lst then lst else bubbleSort $ bubble lst 

-- Version alternativa de la última función (es mas rápida)
bsort :: Ord a => [a] -> Int -> [a]
bsort lst a = if a /= length lst then bsort (bubble lst) (a+1) else lst 
bubbleSort' lst = bsort lst 0



-- Merge Sort
-- Explicación: https://www.youtube.com/watch?v=4VqmGXwpLqc



-- Heap Sort
-- Explicación: https://www.youtube.com/watch?v=2DmK_H7IdTo



-- Quick Sort
-- Explicación: https://www.youtube.com/watch?v=Hoixgm4-P4M





