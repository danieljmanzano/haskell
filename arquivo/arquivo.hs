import System.IO

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

    putStrLn $ show $ (n1, n2, n3, n4)
    putStrLn $ show l
    hClose h

data Info = Info {
    country :: String,
    confirmed :: Int,
    deaths :: Int,
    recovery :: Int,
    active :: Int
} deriving Show

leitura :: String -> [Info]
leitura = map (\line -> -- map de cada linha
    let [country, confirmed, deaths, recovery, active] = split ',' line -- lê os dados da linha separados por vírgula
    in Info country (read confirmed) (read deaths) (read recovery) (read active) -- converte os dados para o tipo Info
    ) . lines 

-- função usada para separar informações separadas por certo delimitador (no caso, aplicamos para vírgula)
-- funciona como a "splitOn", mas para deixar visível o funcionamento, está feita aqui uma versão "alternativa"
split :: Char -> String -> [String]
split _ [] = [""] -- caso base: se a string estiver vazia, retorna uma lista com uma string vazia
split delim (x:xs)
    -- caso o caracter seja o delimitador, "desconsidera" e aplica pro restante da lista
    | x == delim = "":rest 
    -- caso contrário, concatena o caracter com o primeiro elemento da lista (meio que refaz a string original) e aplica pro restante da lista
    | otherwise  = (x : head rest) : tail rest 
      where
        rest = split delim xs
