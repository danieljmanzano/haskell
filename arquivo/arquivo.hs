import System.IO
import Data.List (sortOn)

{-------------------------------------------------
feito por:
    Daniel Jorge Manzano, n°USP: 15446861
    Heitor Gomes de Oliveira, n°USP: 15458350
    Newton Eduardo Pena Villegas, n°USP: 15481732

-------------------------------------------------}

main :: IO()
main = do
    -- abertura do arquivo
    h <- openFile "dados.csv" ReadMode
    contents <- hGetContents h
    let ls = lines contents
    let l = length ls

    -- leitura da linha de entrada
    line <- getLine
    let entrada = words line
    let n1 = read $ head entrada :: Int
        n2 = read $ entrada !! 1 :: Int
        n3 = read $ entrada !! 2 :: Int
        n4 = read $ entrada !! 3 :: Int

    let infos = leitura $ unlines ls -- lê o conteúdo do arquivo e transforma em uma lista de Info
    -- obs.: "unlines" transforma ls em uma única string, em que os elementos se separam por "\n"

    -- a soma de "Active" de todos os países em que "Confirmed" é maior o igual que n1
    putStrLn $ show $ sum $ map active $ filter (\info -> confirmed info >= n1) infos

    -- dentre os n2 países com maiores valores de "Active", a soma das "Deaths" dos n3 países com menores valores de "Confirmed"
    putStrLn $ show $ sum $ map deaths $ take n3 $ sortOn confirmed $ take n2 $ reverse $ sortOn active infos

    -- os n4 países com os maiores valores de "Confirmed". os nomes devem estar em ordem alfabética
    putStr $ unlines $ map country $ sortOn country $ take n4 $ reverse $ sortOn confirmed infos
    -- obs.: unlines, denovo, transforma a lista de strings em uma única string (com separações por \n)
    -- nao se usa putStrLn para não quebrar linha a mais ao fim
    
    hClose h

data Info = Info {
    country :: String,
    confirmed :: Int,
    deaths :: Int,
    recovery :: Int,
    active :: Int
} deriving Show

-- função que transforma uma string (no caso, o conjunto de todas as informações do arquivo) em uma lista de Info
leitura :: String -> [Info]
leitura = map (\line -> -- map de cada linha
    let [country, confirmed, deaths, recovery, active] = split ',' line -- lê os dados da linha separados por vírgula
    in Info country (read confirmed) (read deaths) (read recovery) (read active) -- converte os dados para o tipo Info
    ).lines 


-- função usada para separar informações separadas por certo delimitador (no caso, aplicamos para vírgula)
-- funciona como a "splitOn", mas para deixar visível o funcionamento está feita aqui uma versão "alternativa"
split :: Char -> String -> [String]
split _ [] = [""] -- caso base: se a string estiver vazia, retorna uma lista com uma string vazia
split delim (x:xs)
    -- caso o caracter seja o delimitador, "desconsidera" e aplica pro restante da lista
    | x == delim = "":rest 
    -- caso contrário, concatena o caracter com o primeiro elemento da lista (meio que refaz a string original) e aplica pro restante da lista
    | otherwise  = (x : head rest) : tail rest 
      where
        rest = split delim xs
