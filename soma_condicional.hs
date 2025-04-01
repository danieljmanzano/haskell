main = do

    putStrLn $ show $ filtra (\x -> x > 0) a -- esse "(\x -> x > 0)" é o que se chama de expressão lambda, basicamente uma condicional
    putStrLn $ show $ filtra ehPositivo a -- isso aqui dá no mesmo que de cima, mas o de cima é mais conciso 

    putStrLn $ show $ soma $ filtra (\x -> x > 0) a -- faz a soma da lista filtrada em números positivos de acordo com a condicional esquisita aqui
    putStrLn $ show $ somaSe ehPositivo a -- faz a soma dos positivos da lista, mas usando uma outra função feita "menos genérica" (o somaSe)
-- ambas as soluções resultam no mesmo, mas a solução das linhas 3 e 6 é melhor de acordo com o professor. teoricamente, é "mais funcional", consigo usar funções mais genéricas

    putStrLn $ show $ soma $ filtra (\x -> mod x 2 == 0) a
    putStrLn $ show $ somaSe ehPar a
-- denovo, dá no mesmo. mas a linha 10 é preferível


a = [1, -2, 7, 8, 10, 0, -4, -3, 19] -- lista aleatória. não sei receber na main rsrsrs então to colocando na mão mesmo


-- primeira solução para implementar somas condicionais. não muito boa
ehPositivo x = x > 0
ehNegativo x = x < 0
ehPar x = mod x 2 == 0 -- obs. legal: se fizer 'x `mod` 2' dá no mesmo. colocar uma função prefixa entre crases torna ela infixa
ehImpar x = mod x 2 == 1 

somaSe _ [] = 0 -- função para somar números de acordo com alguma condição que passo
somaSe cond (x:xs) -- no cond será passada alguma das funções de verificação de cima, aí decido o que quero somar
  | cond x = x + somaSe cond xs
  | otherwise = somaSe cond xs


-- daqui pra baixo ta melhor
filtra :: (a -> Bool) -> [a] -> [a]
filtra _ [] = [] -- filtro uma lista de acordo com o que coloco no teste. devolve a lista filtrada
filtra teste (x:xs)
  | teste x = x : r
  | otherwise = r
  where
    r = filtra teste xs

soma :: (Num a) => [a] -> a -- definição: 'a' tem que ser um tipo "somável", então colocamos esse 'Num a' antes
soma [] = 0 -- por fim, uma soma genérica
soma (x:xs) = x + soma xs


-- código que fui fazendo durante a aula (pegando coisa do código do professor também)
-- a ideia principal era entender como "melhorar" implementações usando esses problemas como exemplos