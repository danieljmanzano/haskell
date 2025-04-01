main = do
  putStrLn $ show $ qs $ a


a :: [Integer]
a = [2, 5, 3, 7, 10, -2, 9, 2, -3]

filtra :: (a -> Bool) -> [a] ->[a] -- mesmo filtra do "somaCondicional"
filtra _ [] = []
filtra teste (x:xs) 
  | teste x = x : filtra teste xs
  | otherwise = filtra teste xs

-- nao ironicamente, muito simples
qs :: (Ord a) => [a] -> [a] -- classe de tipos 'Ord' == todo tipo ordenável
qs [] = [] -- sort de uma lista vazia vem lista vazia
qs (x:xs) = menores ++ iguais ++ maiores -- o quicksort da lista é basicamente os menores concatenados (++) com iguais concatenados com maiores
 where
  menores = qs $ filtra (<x) xs -- os menores são ordenados usando o filtra de menores que o pivo x (o <x funciona como teste de numeros menores que x em xs)
  iguais = x : filtra (==x) xs -- os iguais são "mantidos" no lugar após o x
  maiores = qs $ filtra (>x) xs -- os maiores são ordenados também com o filtra de maiores que x

-- implementando o quicksort eeeita lasquera
-- feito em sala