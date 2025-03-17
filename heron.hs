-- código simples que recebe 3 lados de um triângulo, calcula a área dele pela fórmula de Heron e printa
-- caso os lados não formem um triângulo, printo "-"

main = do
    la <- getLine
    lb <- getLine
    lc <- getLine
    let a = read la :: Double
        b = read lb :: Double
        c = read lc :: Double

    putStrLn $ printaResultado $ heron a b c


ehTriangulo :: Double -> Double -> Double -> Bool
ehTriangulo a b c 
  | a < 0 || b < 0 || c < 0 = False -- se houver algum lado negativo, não pode ser triângulo
  | (a + b >= c) && (a + c >= b) && (b + c >= a) = True -- se essa condição for verdadeira, os tamanhos dos lados possibilitam que seja um triângulo
  | otherwise = False -- senão, não é triângulo

heron :: Double -> Double -> Double -> Maybe Double
heron a b c
  | ehTriangulo a b c = Just (sqrt $ s * (s - a) * (s - b) * (s - c)) -- caso a verificação de ehTriangulo seja verdade, calculo a área
  | otherwise = Nothing -- senão, retorno nada
  where
    s = (a + b + c) / 2 -- definição de s, é o semiperímetro

printaResultado :: Maybe Double -> String
printaResultado resultado
  | resultado == Nothing = "-"
  | Just resultadoReal <- resultado = show resultadoReal