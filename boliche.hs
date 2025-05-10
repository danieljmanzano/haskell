-- feito por Daniel

main :: IO ()
main = do
    -- getContents faz a leitura até o fim da entrada
    input <- getContents 

    -- 'words' quebra a entrada em palavras (separadas por espaço)
    -- 'map read' converte cada palavra em um int
    -- 'pontos' é a lista contendo todos os pontos
    let pontos = map read $ words input :: [Int]
    print pontos

-- um Frame pode ser:
data Frame = Strike -- strike: só 1 lançamento de 10 pontos
  | Spare Int Int -- spare: dois lançamentos que somam 10
  | Open Int Int -- open: dois lançamentos que não somam 10
  | Final Int Int (Maybe Int) -- 10º frame com 2 ou 3 jogadas
  deriving (Show)

{-
calcularPontuacao :: [Int] -> Int
calcularPontuacao jogadas = calcular jogadas 1
  where
    -- caso base: após o décimo frame, parar
    calcular _ 11 = 0
    -- caso a jogada seja um strike, soma 10, o bônus de strike e continua a recursão (chama o calcular para o próximo frame / resto da lista)
    calcular (10:xs) frame = 10 + bonusStrike xs + calcular xs (frame + 1)
    -- para o caso de não ser strike
    calcular (a:b:xs) frame 
      -- caso a soma das jogadas seja 10 (spare), soma 10, o bônus de spare e continua a recursão
      | a + b == 10 = 10 + bonusSpare xs + calcular xs (frame + 1)
      -- caso contrário, apenas soma as jogadas e continua a recursão
      | otherwise   = a + b + calcular xs (frame + 1)
    calcular _ _ = 0 -- caso de erro (ou coisa do tipo)

    -- bônus de strike: soma as próximas duas pontuações
    bonusStrike (a:b:_) = a + b
    bonusStrike (a:_)   = a -- caso só exista uma, usa ela
    bonusStrike _       = 0 -- caso não existam jogadas posteriores, retorna 0

    -- bônus de spare: retorna o próximo lançamento após o spare
    bonusSpare (a:_) = a
    bonusSpare _ = 0 -- caso não exista uma jogada posterior, retorna 0
-}

calcularPontuacao :: [Frame] -> Int
calcularPontuacao frames = soma frames
  where
    soma [] = 0
    soma (Strike:rest) = 10 + bonus rest 2 + soma rest
    soma (Spare _ _:rest) = 10 + bonus rest 1 + soma rest
    soma (Open a b:rest) = a + b + soma rest
    soma [Final a b (Just c)] = a + b + c
    soma [Final a b Nothing] = a + b
    soma _ = 0  -- fallback

    bonus :: [Frame] -> Int -> Int
    bonus _ 0 = 0
    bonus [] _ = 0
    bonus (Strike:xs) n = 10 + bonus xs (n - 1)
    bonus (Spare a b:xs) n = a + b + bonus xs (n - 2)
    bonus (Open a b:xs) n = a + b + bonus xs (n - 2)
    bonus (Final a b (Just c):_) n = takeN n [a, b, c]
    bonus (Final a b Nothing:_) n = takeN n [a, b]

    takeN n xs = sum (take n xs)


extrairFrames :: [Int] -> [Frame]
extrairFrames jogadas = extrair jogadas 1
  where
    -- caso base: depois do 10º frame, ignora
    extrair _ 11 = []  
    -- caso em que a jogada (na cabeça da lista) foi um strike
    extrair (10:xs) n
      | n < 10 = Strike : extrair xs (n + 1) -- se a próxima jogada não for o 10º frame, continua a recursão
    -- tratando as duas próximas jogadas da lista
    extrair (a:b:xs) n
      | n < 10 && a + b == 10 = Spare a b : extrair xs (n + 1) -- caso em que a soma das jogadas seja 10 (spare)
      | n < 10 = Open a b : extrair xs (n + 1) -- caso em que a soma das jogadas não seja 10 (open frame)
      | n == 10 = [Final a b (listToMaybe xs)] -- caso em que estamos no 10° frame
    extrair _ _ = [] -- caso de erro (ou coisa do tipo)

