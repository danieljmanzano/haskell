-- devo receber dois numeros. entre eles, devo contar quantos numeros defeituosos, perfeitos e abundantes existem
-- numero defeituoso == a soma de seus divisores (exceto o próprio numero) é menor que ele
-- numero perfeito == a soma de seus divisores é igual a ele
-- numero abundante == a soma de seus divisores é maior que ele

main = do
    la <- getLine
    lb <- getLine
    let a = read la :: Int
        b = read lb :: Int

    putStrLn $ show $ contaNumeros ehDefeituoso [a..b]
    putStrLn $ show $ contaNumeros ehPerfeito [a..b]
    putStrLn $ show $ contaNumeros ehAbundante [a..b]



contaNumeros :: (Int -> Bool) -> [Int] -> Int
contaNumeros _ [] = 0
contaNumeros condicao (x:xs) -- aplico uma condição em cada um dos números presentes na minha lista. caso ela seja verdadeira, adiciona na contagem
  | condicao x = 1 + contaNumeros condicao xs
  | otherwise = contaNumeros condicao xs


-- essas tres embaixo sao as condiçoes. nao tem como ser mais basico, entao nem vou explicar
ehDefeituoso :: Int -> Bool
ehDefeituoso a = somaDivisores a [1..(a-1)] < a


ehPerfeito :: Int -> Bool
ehPerfeito a = somaDivisores a [1..(a-1)] == a


ehAbundante :: Int -> Bool
ehAbundante a = somaDivisores a [1..(a-1)] > a


-- denovo, acho que ja ta bem obvio pra ter que explicar
somaDivisores :: Int -> [Int] -> Int
somaDivisores a [] = 0
somaDivisores a (x:xs)
  | mod a x == 0 = x + somaDivisores a xs
  | otherwise = somaDivisores a xs