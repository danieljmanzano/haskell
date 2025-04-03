main = do
    putStrLn $ show $ pares a b
    putStrLn $ show $ primeiro (23, 42)
    putStrLn $ show $ segundo ("abroba", "torresmo")

a :: [Integer]
a = [1, 3, 5, 7, 9]

b :: [Integer]
b = [2, 4, 6, 8, 10]

pares :: [a] -> [b] -> [(a,b)] -- funçao que faz pares (tuplas) com duas listas
pares _ [] = [] -- caso acabe a primeira lista eu paro, nao tem mais como fazer par
pares [] _ = [] -- mesmo de cima com a segunda lista
pares (x:xs)(y:ys) = (x, y) : pares xs ys

primeiro :: (a, b) -> a -- retorna o primeiro elemento da tupla
primeiro (a, _) = a

segundo :: (a, b) -> b -- retorna o segundo elemento da tupla
segundo (_, b) = b

-- aprendendo a mexer com tuplas. bem do basico, deixando aqui pra usar como referencia se precisar mais pra frente