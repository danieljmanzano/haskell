-- feito por Daniel
{- o programa simula um jogo de boliche, processando uma lista de lançamentos 
e calculando a pontuação de acordo com as regras oficiais -}
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
    -- getContents faz a leitura até o fim da entrada
    input <- getContents 

    -- 'words' quebra a entrada em palavras (separadas por espaço)
    -- 'map read' converte cada palavra em um int
    -- 'pontos' é a lista contendo todos os pontos de cada lançamento
    let pontos = map read $ words input :: [Int]
    putStrLn $ mostrarJogo pontos

-- um Frame pode ser um dos seguintes tipos:
data Frame = Strike -- strike: só 1 lançamento de 10 pontos
  | Spare Int Int -- spare: dois lançamentos que somam 10
  | Open Int Int -- open: dois lançamentos que não somam 10
  | Final Int Int (Maybe Int) -- 10º frame com 2 ou 3 jogadas
  deriving (Show)

calcularPontuacao :: [Frame] -> Int
-- calcula a pontuação total do jogo com base na lista de frames
calcularPontuacao frames = sum $ scoreFrames frames
  where
    -- scoreFrames: calcula a pontuação de cada frame e retorna uma lista de pontuações
    -- para cada frame, calcula a pontuação base + o bônus correspondente
    -- entra uma lista de frames, retorna uma lista de pontuações de cada frame
    scoreFrames :: [Frame] -> [Int]
    scoreFrames [] = [] -- caso base: lista vazia retorna lista vazia
    scoreFrames [Final a b (Just c)] = [a + b + c] -- caso do 10º frame com 3 lançamentos
    scoreFrames [Final a b Nothing] = [a + b] -- caso do 10º frame com 2 lançamentos
    
    -- Strike: 10 pontos + bônus dos dois próximos lançamentos
    scoreFrames (Strike:xs) = (10 + strikeBonus xs) : scoreFrames xs
    
    -- Spare: 10 pontos + bônus do próximo lançamento
    scoreFrames (Spare _ _:xs) = (10 + spareBonus xs) : scoreFrames xs
    
    -- Open: simplesmente soma dos dois valores
    scoreFrames (Open a b:xs) = (a + b) : scoreFrames xs
    
    -- strikeBonus: calcula o bônus específico para um Strike (os próximos dois lançamentos)
    -- entra uma lista de frames (os frames após o strike), retorna o valor do bônus
    strikeBonus :: [Frame] -> Int
    strikeBonus [] = 0 -- não há frames após, então não há bônus
    strikeBonus [Final a b _] = a + b -- se o próximo é o frame final, o bônus é a soma dos dois primeiros lançamentos
    strikeBonus (Strike:xs) = 10 + lancamento1 xs -- se o próximo também for Strike, é 10 + o primeiro lançamento do frame seguinte
    strikeBonus (Spare a _:_) = 10 -- se o próximo for Spare, somamos os 10 pontos do spare como bônus
    strikeBonus (Open a b:_) = a + b -- se o próximo for Open, o bônus é a soma dos dois lançamentos
    
    -- spareBonus: calcula o bônus específico para um Spare (o próximo lançamento)
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

-- adapta a lista de lançamentos para uma lista de frames que vão ser usados para avaliar as jogadas mais corretamente
-- converte uma lista simples de números em uma lista estruturada de frames
extrairFrames :: [Int] -> [Frame]
extrairFrames jogadas = extrair jogadas 1
  where
    -- extrair: função auxiliar recursiva que processa a lista de jogadas
    -- recebe a lista de jogadas restantes e o número do frame atual
    -- retorna uma lista de Frames processados
    
    -- caso base: depois do 10º frame, ignora o resto
    extrair _ 11 = []  
    
    -- caso em que a jogada (na cabeça da lista) foi um strike
    extrair (10:xs) n
      | n < 10 = Strike : extrair xs (n + 1) -- se não for o 10º frame, continua normalmente
      | n == 10 && not (null xs) = -- tratamento especial para strike no 10º frame
          let (finalFrame, _) = extrairFinal10 (10:xs) -- usa a função auxiliar para extrair o 10º frame
          in [finalFrame]
    
    -- tratando jogadas normais (dois lançamentos por vez)
    extrair (a:b:xs) n
      | n < 10 && a + b == 10 = Spare a b : extrair xs (n + 1) -- caso de spare
      | n < 10 = Open a b : extrair xs (n + 1) -- caso de open frame
      | n == 10 = [Final a b (if a + b >= 10 then listToMaybe xs else Nothing)] -- 10º frame: 3ª jogada só é permitida se houver spare ou strike
    
    extrair _ _ = [] -- caso de erro ou lista insuficiente
    
    -- extrairFinal10: função auxiliar específica para processar o 10º frame
    -- recebe a lista de jogadas começando do 10º frame e retorna o frame final e as jogadas restantes
    extrairFinal10 :: [Int] -> (Frame, [Int])
    extrairFinal10 (10:b:c:xs) = (Final 10 b (Just c), xs) -- strike no 10º frame: tem direito a mais duas jogadas
    extrairFinal10 (10:b:xs) = (Final 10 b Nothing, xs) -- strike no 10º frame mas só fez mais uma jogada
    extrairFinal10 (a:b:c:xs) | a + b == 10 = (Final a b (Just c), xs) -- spare no 10º frame: tem direito a mais uma jogada
    extrairFinal10 (a:b:xs) = (Final a b Nothing, xs) -- jogada normal no 10º frame: só duas jogadas
    extrairFinal10 xs = (Final 0 0 Nothing, xs) -- caso de erro (lista insuficiente)





-- daqui pra baixo é as funções de print. estão meio erradas, então só tem que arrumar isso aqui pra baixo
mostrarJogo :: [Int] -> String
-- formata o jogo para exibição, incluindo todos os frames e o total
-- recebe a lista de lançamentos, retorna uma string formatada
mostrarJogo jogadas =
  let frames = extrairFrames jogadas -- converte a lista de números em frames estruturados
      partes = map mostrarFrameFormatado frames -- formata cada frame para exibição
      total = calcularPontuacao frames -- calcula a pontuação total
  in unwords partes ++ " " ++ show total -- junta tudo em uma string

mostrarFrameFormatado :: Frame -> String
-- formata cada tipo de frame em uma representação de string amigável
-- recebe um frame, retorna uma string formatada
mostrarFrameFormatado Strike = "X _ |" -- strike é representado por X
mostrarFrameFormatado (Spare a b) = show a ++ " / |" -- spare usa / para o segundo lançamento
mostrarFrameFormatado (Open a b) = show a ++ " " ++ show b ++ " |" -- frame normal mostra os dois valores
mostrarFrameFormatado (Final a b (Just c)) =
  formatarFinal a ++ " " ++ formatarFinal b ++ " " ++ formatarFinal c ++ " |" -- 10º frame com 3 lançamentos
mostrarFrameFormatado (Final a b Nothing) =
  formatarFinal a ++ " " ++ formatarFinal b ++ " _ |" -- 10º frame com 2 lançamentos

formatarFinal :: Int -> String
-- formata um lançamento individual (especialmente para o 10º frame)
-- recebe um valor de lançamento, retorna uma string formatada
formatarFinal 10 = "X" -- valor 10 é mostrado como X
formatarFinal x = show x -- outros valores são mostrados normalmente