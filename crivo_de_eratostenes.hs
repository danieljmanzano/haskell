main = do
--  putStrLn $ show $ pega 10 primos
    putStrLn $ show $ primos

primos = crivo [2..]
  where
    crivo (x:xs) = x : crivo (filtra (\y -> y `mod` x /= 0) xs)
    -- explicando essa esquisitisse de cima: o crivo é uma lista que começa em 2 (e vai pro infinito ainda)
    -- aí, a definiçao dele é que, para todo numero y que nao seja divisivel pelo x atual (cabeça) da lista do crivo, inclui ele na lista (definido na funçao lambda)
    -- é estranho de pensar onde entra o y, mas ele é passado porque a lista teoricamente nao ta bem definida (aí ela meio que se define sozinha - e define o y - quando quero usar ela)
    -- se quiser dá pra dale quantos primos quiser porque a propria lista do crivo nao tem limite, só tirar o pega na main (aí ele printa todos existentes até nao aguentar mais)


    -- outra forma de se aplicar
--where
--  crivo (x:x) = x : crivo (filtra ((/= 0).(`mod` x)) xs)
    -- nessa aqui, estou fazendo uma funçao composta que basicamente faz 'numero mod x != 0'. mais "elegante"
    -- o '.' funciona como a bola de matematica das funçoes quando faço um f bola g (f o g, equivalente ao f(g(x)))
    -- o (/= 0) é como a funçao lambda la de cima so que mais sucinto, serve pra mesma coisa


-- funçao pega: passa um numero e uma lista, retorna os n primeiros numeros da lista
pega :: Integer -> [a] -> [a]
pega _ [] = [] -- pegar de uma lista vazia retorna ela vazia
pega 0 _ = [] -- pega 0 retorna uma lista vazia
pega n (x:xs) = x : pega (n - 1) xs

-- funçao filtra: retorna uma lista com apenas os elementos que satisfazem o teste passado com ela
filtra :: (a -> Bool) -> [a] -> [a]
filtra _ [] = [] -- filtrar de lista vazia nao retorna
filtra teste (x:xs) 
  | teste x = x : filtra teste xs -- caso teste seja true, junta ele no 'filtra (restante da lista)'
  | otherwise = filtra teste xs