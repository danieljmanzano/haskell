main = do
    putStrLn $ show $ "hora de acordar no " ++ show Dom
    putStrLn $ show $ horaDeAcordar Dom
    


-- criando um tipo para os dias da semana
data DiaDaSemana = Dom | Seg | Ter | Qua | Qui | Sex | Sab

-- para que possamos fazer um "show" do dia da semana, precisamos implementar o tipo em função da classe Show
instance Show DiaDaSemana where
    show Dom = "Domingo"
    show Seg = "Segunda"
    show Ter = "Terça"
    show Qua = "Quarta"
    show Qui = "Quinta"
    show Sex = "Sexta"
    show Sab = "Sábado"

horaDeAcordar :: DiaDaSemana -> Integer
horaDeAcordar Dom = 7
horaDeAcordar Sab = 9
horaDeAcordar _ = 6

