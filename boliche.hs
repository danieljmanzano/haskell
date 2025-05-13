{-------------------------------------------------
feito por:
    Daniel Jorge Manzano, n°USP: 15446861
    Heitor Gomes de Oliveira, n°USP: 15458350
    Newton Eduardo Pena Villegas, n°USP: 15481732

-------------------------------------------------}

main :: IO ()
main = do
    -- getContents faz a leitura até o fim da entrada
    input <- getContents 

    -- 'words' quebra a entrada em palavras (separadas por espaço)
    -- 'map read' converte cada palavra em um int
    -- 'pontos' é a lista contendo todos os pontos
    let pontos = map read $ words input :: [Int]
    let frames = extrairFrames pontos -- extrai os frames da lista de pontos
    putStr $ formatar frames -- formata os frames para impressão
    putStr " | " -- imprime um espaço em branco
    print $ calcularPontuacao frames -- calcula a pontuação e imprime o resultado


-- um Frame pode ser:
data Frame = Strike -- strike: só 1 lançamento de 10 pontos
  | Spare Int Int -- spare: dois lançamentos que somam 10
  | Open Int Int -- open: dois lançamentos que não somam 10
  | Final Int Int (Maybe Int) -- 10º frame com 2 ou 3 jogadas



{------ FUNÇÕES PARA OS CÁLCULOS DOS PONTOS -------}

-- função originalmente do "Data.Maybe", usada como auxiliar mais pra frente. implementado aqui para deixar visível como funciona
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing -- caso a lista esteja vazia, retorna Nothing
listToMaybe (x:_) = Just x -- caso a lista tenha pelo menos um elemento, retorna o primeiro elemento

-- função que adapta a lista de lançamentos para uma lista de frames que vão ser usados para avaliar as jogadas mais corretamente
extrairFrames :: [Int] -> [Frame]
extrairFrames jogadas = extrair jogadas 1
  where
    -- caso base: depois do 10º frame, ignora o resto
    extrair _ 11 = []  
    
    -- caso em que a jogada (na cabeça da lista) foi um strike
    extrair (10:xs) n
      | n < 10 = Strike : extrair xs (n + 1) -- se não for o 10º frame, continua normalmente
      | n == 10 = -- tratamento especial para strike no 10º frame
        let (finalFrame, _) = extrairFinal10 (10:xs) -- usa a função auxiliar para extrair o 10º frame
        in [finalFrame]
    
    -- tratando jogadas normais (dois lançamentos por vez)
    extrair (a:b:xs) n
      | n < 10 && a + b == 10 = Spare a b : extrair xs (n + 1) -- caso de spare
      | n < 10 = Open a b : extrair xs (n + 1) -- caso de open frame
      | n == 10 = [Final a b (listToMaybe xs)] -- 10º frame: 3ª jogada só é permitida se houver spare ou strike

    extrair _ _ = [] -- caso de erro ou lista insuficiente
    
    -- extrairFinal10: função auxiliar específica para processar o 10º frame
    -- recebe a lista de jogadas começando do 10º frame e retorna o frame final e as jogadas restantes
    extrairFinal10 :: [Int] -> (Frame, [Int])
    extrairFinal10 (10:b:c:xs) = (Final 10 b (Just c), xs) -- strike no 10º frame: tem direito a mais duas jogadas
    extrairFinal10 (10:b:xs) = (Final 10 b Nothing, xs) -- strike no 10º frame mas só fez mais uma jogada
    extrairFinal10 (a:b:c:xs) | a + b == 10 = (Final a b (Just c), xs) -- spare no 10º frame: tem direito a mais uma jogada
    extrairFinal10 (a:b:xs) = (Final a b Nothing, xs) -- jogada normal no 10º frame: só duas jogadas
    extrairFinal10 xs = (Final 0 0 Nothing, xs) -- caso de erro (lista insuficiente)

-- função "principal" que calcula a pontuação total do jogo a partir de uma lista com os frames do jogo
calcularPontuacao :: [Frame] -> Int
calcularPontuacao frames = sum $ scoreFrames frames
  where
    -- scoreFrames: calcula a pontuação de cada frame e retorna uma lista de pontuações
    -- para cada frame, calcula a pontuação base + o bônus correspondente
    scoreFrames :: [Frame] -> [Int]
    scoreFrames [] = [] -- caso base: lista vazia retorna lista vazia
    scoreFrames [Final a b (Just c)] = [a + b + c] -- caso do 10º frame com 3 lançamentos
    scoreFrames [Final a b Nothing] = [a + b] -- caso do 10º frame com 2 lançamentos
    scoreFrames (Strike:xs) = (10 + strikeBonus xs) : scoreFrames xs -- strike: 10 pontos + bônus dos dois próximos lançamentos
    scoreFrames (Spare _ _:xs) = (10 + spareBonus xs) : scoreFrames xs -- spare: 10 pontos + bônus do próximo lançamento
    scoreFrames (Open a b:xs) = (a + b) : scoreFrames xs -- open: simplesmente, soma dos dois valores
    
    -- strikeBonus: calcula o bônus específico para um strike (os próximos dois lançamentos)
    -- entra uma lista de frames (os frames após o strike), retorna o valor do bônus
    strikeBonus :: [Frame] -> Int
    strikeBonus [] = 0 -- não há frames após, então não há bônus
    strikeBonus (Final a b _:_) = a + b -- se o próximo é o frame final, o bônus é a soma dos dois primeiros lançamentos
    strikeBonus (Strike:xs) = 10 + lancamento1 xs -- se o próximo também for strike, é 10 + o primeiro lançamento do frame seguinte
    strikeBonus (Spare a _:_) = 10 -- se o próximo for spare, soma os 10 pontos do spare como bônus
    strikeBonus (Open a b:_) = a + b -- se o próximo for open, o bônus é a soma dos dois lançamentos
    
    -- spareBonus: calcula o bônus específico para um spare (o próximo lançamento)
    -- entra uma lista de frames (os frames após o spare), retorna o valor do bônus
    spareBonus :: [Frame] -> Int
    spareBonus [] = 0 -- não há frames após, então não há bônus
    spareBonus [Final a _ _] = a -- se o próximo é o frame final, o bônus é apenas o primeiro lançamento
    spareBonus (Strike:_) = 10 -- se o próximo for strike, o bônus é 10
    spareBonus (Spare a _:_) = a -- se o próximo for spare, o bônus é o primeiro lançamento do spare
    spareBonus (Open a _:_) = a -- se o próximo for open, o bônus é o primeiro lançamento
    
    -- lancamento1: função auxiliar para obter o valor do primeiro lançamento de um frame
    -- usada para calcular bônus em casos complexos (ex: strike seguido de strike)
    -- entra uma lista de frames, retorna o valor do primeiro lançamento
    lancamento1 :: [Frame] -> Int
    lancamento1 [] = 0 -- caso base: não há frames, retorna 0
    lancamento1 [Final a _ _] = a -- caso do frame final: retorna o primeiro lançamento
    lancamento1 (Strike:_) = 10 -- caso de strike: o primeiro lançamento vale 10
    lancamento1 (Spare a _:_) = a -- caso de spare: retorna o primeiro lançamento
    lancamento1 (Open a _:_) = a -- caso de open: retorna o primeiro lançamento



{------ FUNÇÕES PARA O PRINT DOS RESULTADOS -------}

formatar :: [Frame] -> String                                      -- Função que formata uma lista de frames em uma string
formatar [] = ""                                                   -- Caso base: lista vazia, retorna string vazia
formatar (Strike:rest) = "X _ | " ++ formatar rest                 -- Strike: exibe "X _" e continua formatando o resto
formatar (Spare a b:rest) = show a ++ " / | " ++ formatar rest     -- Spare: exibe primeiro valor e "/" para indicar spare
formatar (Open a b:rest) = show a ++ " " ++ show b ++ " | " ++ formatar rest  -- Frame aberto: exibe os dois valores

formatar [Final a b (Just c)]                                      -- Último frame com terceiro lançamento
  | a == 10 && b == 10 && c == 10 = "X X X"                        -- Três strikes no final
  | a == 10 && b == 10            = "X X " ++ mostrarLancamento c  -- Dois strikes e um terceiro lançamento
  | a == 10 && b + c == 10 && b /= 10 = "X " ++ mostrarLancamento b ++ " /"  -- Strike seguido de spare (sem segundo strike)
  | a == 10                       = "X " ++ mostrarLancamento b ++ " " ++ mostrarLancamento c  -- Strike seguido de dois lançamentos comuns
  | a + b == 10                   = show a ++ " / " ++ mostrarLancamento c  -- Spare seguido de um terceiro lançamento
  | otherwise                     = show a ++ " " ++ show b ++ " " ++ mostrarLancamento c      -- Nenhum strike/spare: mostra os três valores

formatar [Final a b Nothing]                                       -- Último frame com apenas dois lançamentos
  | a == 10 && b == 10 = "X X"                                     -- Dois strikes
  | a == 10            = "X " ++ mostrarLancamento b               -- Um strike seguido de um lançamento comum
  | a + b == 10        = show a ++ " /"                            -- Spare
  | otherwise          = show a ++ " " ++ show b                   -- Dois lançamentos comuns

-- Função auxiliar para mostrar lançamentos com símbolo X quando for 10
mostrarLancamento :: Int -> String
mostrarLancamento 10 = "X"
mostrarLancamento n  = show n
