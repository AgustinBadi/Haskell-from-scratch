fst' :: (a, b) -> a
fst' (a, b) = a

snd' :: (a, b) -> b 
snd' (a, b) = b 

zip' :: [a] -> [b] -> [(a, b)]
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys) 