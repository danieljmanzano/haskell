-- devo receber dois números ('a' e 'b') e calcular o maior "comprimento" entre dois primos consecutivos que estejam dentro da faixa [a..b]
-- pra fazer isso, eu:
--   pego duas listas de primos (uma com [a..b] e outra [a'..b], em que a' é o primo imediatamente maior que a),
--   faço uma lista de tuplas entre essas duas listas de primos,
--   pego as diferenças entre os valores das tuplas
--   e, por fim, pego o maior valor da lista de diferenças

main = do
    la <- getLine
    lb <- getLine
    let a = read la :: Int
        b = read lb :: Int

    let primos1 = filtra (\x -> x >= a && x <= b) (takeWhile (<= b) primos) :: [Int] -- lista com os primos [a..b]. para quando passa de b (pra isso o 'takeWhile')
        primos2 = drop 1 primos1 -- lista com os primos [a..b] sem o primeiro elemento (para fazer a diferença entre os primos consecutivos)

    putStrLn $ show $ maior $ subtraiListadeTuplas $ pares primos1 primos2 -- printa o maior comprimento entre primos consecutivos


-- função que retorna o maior inteiro de uma lista. basicamente, só usa a função maximum (nativa), mas coloco a verificação de lista vazia
maior :: [Int] -> Int
maior [] = 0
maior (x:xs) = maximum (x:xs) -- função que retorna o maior elemento de uma lista


-- função que retorna uma lista de tuplas formadas pelos elementos da primeira lista com os da segunda
pares :: [a] -> [b] -> [(a,b)] -- funçao que faz pares (tuplas) com duas listas
pares _ [] = [] -- caso acabe a primeira lista eu paro, nao tem mais como fazer par
pares [] _ = [] -- mesmo de cima com a segunda lista
pares (x:xs)(y:ys) = (x, y) : pares xs ys


-- função que, dada uma lista de tuplas, retorna uma lista com a diferença entre os elementos de cada tupla
subtraiListadeTuplas :: [(Int, Int)] -> [Int]
subtraiListadeTuplas [] = []
subtraiListadeTuplas ((x,y):xs) = (y - x) : subtraiListadeTuplas xs


-- usando a ideia do crivo de Eratóstenes para gerar os números primos
primos :: [Int]
primos = crivo [2..]
  where
    crivo (x:xs) = x : crivo (filtra (\y -> y `mod` x /= 0) xs)


-- funçao que retorna uma lista com apenas os elementos que satisfazem o teste passado com ela
filtra :: (a -> Bool) -> [a] -> [a]
filtra _ [] = [] -- filtrar de lista vazia nao retorna
filtra teste (x:xs) 
  | teste x = x : filtra teste xs -- caso teste seja true, junta ele no 'filtra (restante da lista)'
  | otherwise = filtra teste xs