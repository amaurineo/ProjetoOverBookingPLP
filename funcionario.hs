module Funcionario where
import Util
import Mensagens
import Data.List
import System.IO

-- Recebe cpf do funcionário
cpfFuncionario :: IO()
cpfFuncionario = do
    putStrLn "\nInforme seu CPF para fazer o login: "

-- Verifica se o cpf está cadastrado no sistema
verificaFuncionario :: (IO()) -> IO()
verificaFuncionario menu = do 
    cpfFuncionario
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/funcionarios.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    if Util.temCadastro cpf lista
        then do {putStr"\nBem vindo de volta!\n"; logaFuncionario menu}
    else do
        {Mensagens.usuarioInvalido; menu}

--realiza o login do funcionario
logaFuncionario :: IO() -> IO()
logaFuncionario menu = do
    Mensagens.menuFuncionario

    putStrLn"Opção: "
    op <- Util.lerEntradaString
    if op == "1"
        then do {Util.exibirAssentos; logaFuncionario menu}
    else if op == "2"
        then do {Funcionario.escolheAssento; logaFuncionario menu}
    else if op == "3"
        then do {Mensagens.exibirListaClientesCadastrados; logaFuncionario menu}
    else if op == "4"
        then do {excluirCliente2 menu; logaFuncionario menu}
    else if op == "5"
        then do {calcularValorPassagem menu}
    else if op == "6"
        then do {Funcionario.cadastrarCliente menu; logaFuncionario menu}
    else if op == "7"
        then do menu
    else do
        {Mensagens.opcaoInvalida; logaFuncionario menu}

escolheAssento :: IO()
escolheAssento = do
    putStrLn"\nSerá necessário realizar o cadastro ou log in no sistema!\n"
    Mensagens.getCpf
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/cpf-assento.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    arq2 <- readFile "arquivos/clientes.txt"
    let lista2 = Data.List.map (Util.split(==',') ) (lines arq2)

    if Util.temCadastro cpf lista
        then do Mensagens.usuarioAssentoOcupado
    else if Util.temCadastro cpf lista2
        then do
            putStrLn"\nVocê já está cadastrado, pode continuar.\n"

            putStr"\n"
            Util.escolheAssento cpf
            putStr""
    else do
        putStrLn"Informe o nome: "
        nome <- Util.lerEntradaString

        let clienteStr = cpf ++ "," ++ nome ++ "," ++ "\n"
        appendFile "arquivos/clientes.txt" clienteStr

        Util.escolheAssento cpf
    putStr""

--TALVEZ isso cause um erro
getlines :: Handle -> IO [String]
getlines h = hGetContents h >>= return . lines

-- exclusão do cliente
excluirCliente :: IO() -> IO()
excluirCliente menu = do
    putStrLn"Informe o CPF do cliente que deseja excluir: "
    cpf <- Util.lerEntradaString

    arq <- openFile "arquivos/clientes.txt" ReadMode
    xs <- getlines arq
    let lista = Data.List.map (split(==',') ) xs
    putStr"\nAtualmente temos os seguintes clientes no sistema: "
    print lista

    if not (Util.temCadastro cpf lista)
        then do {Mensagens.usuarioInvalido; logaFuncionario menu}
    else do
        putStr""
        let clientesExc = Util.primeiraCliente (Util.opcaoAssento cpf lista)
        Util.escreveCliente ""

        appendFile "arquivos/clientes.txt" clientesExc

        putStr"\nCliente excluído com sucesso!\n"



aux :: String -> [String] -> Bool
aux v (x:xs) = v == x

calcularValorPassagem :: IO() -> IO()
calcularValorPassagem menu = do
    Mensagens.getCpf
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/clientes.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    if Util.temCadastro cpf lista
        then do {calculo cpf; logaFuncionario menu}
    else do
        {Mensagens.usuarioInvalido; logaFuncionario menu}

calculo :: String -> IO()
calculo cpf = do
    arq <- readFile "arquivos/horario-cpf.txt"
    let lista = Data.List.map (split(==',') ) (lines arq)
    let servicoFilt = Util.auxRecomendar cpf lista

    if lista == []
        then do Mensagens.usuarioInvalido
    else do

        let getVaga = Util.getIndiceCpv(Util.getVagaCpv cpf lista) ++ "," ++ "\n"
        appendFile "arquivos/assentos.txt" getVaga

        Util.escreverCpv (primeiraCpv (Util.opcaoAssento cpf lista))

        Util.escreverHorarioCpf (primeiraCpv (Util.opcaoAssento cpf lista))

        arqClientes <- readFile "arquivos/clientes.txt"
        let lista2 = Data.List.map (split(==',') ) (lines arqClientes)

        valor <- readFile "arquivos/valorAssento.txt"
        let lista3 = lines valor

        Mensagens.valorPago cpf lista2
        print(valorFinalEst "20" (dizHoraInt (horaCpf cpf lista)) (extraInt servicoFilt) (toInt(lista3 !! 0)))

        putStr""


primeiraCpv :: [[String]] -> String
primeiraCpv [] = ""
primeiraCpv (x:xs) = head x ++ "," ++ (x !! 1) ++ "," ++ (x !! 2) ++ "," ++ (x !! 3) ++ "," ++ (x !! 4) ++ "\n" ++ primeiraCpv xs

valorFinalEst :: String -> Int -> Int -> Int -> Int
valorFinalEst saida entrada extra getValor =  (((toInt saida) - entrada) * getValor) + extra

toInt :: String -> Int
toInt s = read (s) :: Int

dizHoraInt:: [[String]] -> Int
dizHoraInt lista = read (lista !! 0 !! 3) :: Int


horaCpf :: String -> [[String]] -> [[String]]
horaCpf _ [] = []
horaCpf v (x:xs) | (auxHoraCpf v x) == False = horaCpf v xs
                   | otherwise = x:horaCpf v xs

extraInt :: [[String]] -> Int
extraInt (x:xs) | (x !! 4) == "s" = 15
                | otherwise = 0

auxHoraCpf :: String -> [String] -> Bool
auxHoraCpf v (x:xs) = (v == x)


getLinesClientes :: Handle -> IO [String]
getLinesClientes h = hGetContents h >>= return . lines


excluirCliente2 :: (IO()) -> IO()
excluirCliente2 menu = do
    arquivo <- openFile "arquivos/clientes.txt" ReadMode 
    linhasCli <- getLinesClientes arquivo
    --let listaDeCliente = ((Data.List.map (splitLacerda(==","))linhas))
    let listaDeCliente = ((Data.List.map (split(==',') ) linhasCli))
    putStr"\nAtualmente temos os seguintes clientes cadastrados:"
    print(listaDeCliente)

    putStrLn"Informe o CPF do cliente que deseja excluir:"
    cpf <- Util.lerEntradaString 
    print(cpf)
    if not (Util.temCadastro cpf listaDeCliente)
        then do {Mensagens.usuarioInvalido; excluirCliente2 menu}
    else do
        let clientes = Util.primeiraHorarioCpf (Util.opcaoAssento cpf listaDeCliente)
        Util.escreveCliente ""
        appendFile "arquivos/clientes.txt" (clientes)
        Mensagens.clienteExcluido


splitLacerda     :: (Char -> Bool) -> String -> [String]
splitLacerda p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

cadastrarCliente :: (IO()) -> IO()
cadastrarCliente menu = do
    Mensagens.cadastrarNome
    nome <- Util.lerEntradaString

    Mensagens.informeCpf
    cpf <- Util.lerEntradaString

    Mensagens.informeIdade
    idade <-Util.lerEntradaString

    arq <- readFile "arquivos/clientes.txt"
    let lista = ((Data.List.map (Util.split(==',') ) (lines arq)))

    if Util.temCadastro cpf lista
       then do {Mensagens.usuarioCadastrado; logaFuncionario menu}
    else do
        let clienteStr = cpf ++ "," ++ idade ++ "\n"
        appendFile "arquivos/clientes.txt" (clienteStr)
        Mensagens.cadastroEfetuado
        logaFuncionario menu