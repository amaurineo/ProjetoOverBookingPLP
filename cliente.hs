module Cliente where
import Util
import Mensagens
import Data.List
import System.IO
    ( hClose,
      hFlush,
      openFile,
      hGetContents,
      hPutStr,
      Handle,
      IOMode(ReadMode, WriteMode) )

--- O cliente pode acessar pelo login ou se cadastrar ---
acessoCliente :: (IO()) -> IO()
acessoCliente menu = do
    Mensagens.loginouCadastroCliente
    escolha <- Util.lerEntradaString

    if(escolha == "1")
        then do verificaCliente menu
    else if(escolha == "2")
        then do cadastrarCliente menu
    else do
        Mensagens.opcaoInvalida
        acessoCliente menu

--- Cadasto de Cliente no sistema ---
cadastrarCliente :: (IO()) -> IO()
cadastrarCliente menu = do
    Mensagens.cadastrarNome
    nome <- Util.lerEntradaString

    Mensagens.informeCpf
    cpf <- Util.lerEntradaString

    Mensagens.informeIdade
    idade <-Util.lerEntradaString

    arq <- readFile "arquivos/clientes.txt"
    let lista = ((Data.List.map (Util.wordsWhen(==',') ) (lines arq)))

    if (Util.ehCadastrado cpf lista)
       then do {Mensagens.usuarioCadastrado; acessoCliente menu}
    else do
        let clienteStr = cpf ++ "," ++ nome ++ "," ++ idade ++ "\n"
        appendFile "arquivos/clientes.txt" (clienteStr)   
        loginCliente menu

--realiza o login do cliente
loginCliente :: (IO()) -> IO()
loginCliente menu = do
    Mensagens.menuCliente

    putStrLn"Opção: "
    op <- Util.lerEntradaString
    if op == "1"
        then do {Util.exibirAssentos; loginCliente menu} --- alterar cadastro
    else if op == "2"
        then do {excluirFuncionario menu; loginCliente menu} --- deleta cadastro
    else if op == "3"
        then do {listaTodosAssentosDisponiveis menu; loginCliente menu} --- exibir assentos disponiveis
    else if op == "4"
        then do menu
    else do
        {Mensagens.opcaoInvalida; loginCliente menu}

-- Verifica se o cpf está cadastrado no sistema
verificaCliente :: (IO()) -> IO()
verificaCliente menu = do
    Mensagens.getCpf 
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/clientes.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    if Util.temCadastro cpf lista
        then do {putStr"\nBem vindo de volta!\n"; loginCliente menu}
    else do
        {Mensagens.usuarioInvalido; menu}

-- alterar cadastro do cliente
alteradaDadoCliente :: (IO()) -> IO()
alteradaDadoCliente menu = do
    Mensagens.alteraNomeouIdade
    escolha <- Util.lerEntradaString
    if(escolha == "1")
        then do novoNome menu
    else if(escolha == "2")
        then do novaIdade menu
    else do
        Mensagens.opcaoInvalida
        acessoCliente menu

novoNome :: (IO()) -> IO()
novoNome menu = do
    putStrLn("Novo nome: ")
    nome <- Util.lerEntradaString

    arq <- openFile "arquivos/clientes.txt" WriteMode
    hPutStr arq nome
    hFlush arq
    hClose arq

    putStr("\nNome alterado com sucesso!\n")

novaIdade :: (IO()) -> IO()
novaIdade menu = do
    putStrLn("Nova Idade: ")
    idade <- Util.lerEntradaString

    arq <- openFile "arquivos/clientes.txt" WriteMode
    hPutStr arq idade
    hFlush arq
    hClose arq

    putStr("\nIdade alterada com sucesso!\n")

-- #
getLinesClientes :: Handle -> IO [String]
getLinesClientes h = hGetContents h >>= return . lines

excluirFuncionario:: (IO()) -> IO()
excluirFuncionario menu = do
                arquivo <- openFile "arquivos/clientes.txt" ReadMode
                linhasCliente <- getLinesClientes arquivo
                let listaDeCliente = ((Data.List.map (split(==',') ) linhasCliente))
                putStr("\nAtualmente temos os seguintes funcionários no sistema: ")
                print(listaDeCliente)

                putStr("\nInforme o CPF do Cliente que deseja excluir: ")
                cpf <- Util.lerEntradaString
                print(cpf)
                if not (Util.temCadastro cpf listaDeCliente)
                    then do {Mensagens.usuarioInvalido; excluirFuncionario menu}
                else do
                    let clientes = Util.primeiraHorarioCpf (opcaoVaga cpf listaDeCliente)
                    Util.escreveCliente ""

                    appendFile "arquivos/clientes.txt" (clientes)
                    Mensagens.funcionarioExcluido



--Listar assentos executivos e econômico disponíveis
getLinesAssentos :: Handle -> IO [String]
getLinesAssentos h = hGetContents h >>= return . lines


listaTodosAssentosDisponiveis:: (IO()) -> IO()
listaTodosAssentosDisponiveis menu = do
                arquivo <- openFile "arquivos/assentos.txt" ReadMode
                linhasAssentos <- getLinesAssentos arquivo
                let listaDeAssentos = ((Data.List.map (split(==',') ) linhasAssentos))
                putStr("\nAtualmente temos os seguintes assentos executivos e econômicos no sistema: \n")
                print(listaDeAssentos)

escolheAssento :: IO()
escolheAssento = do
    putStr"\nSerá necessário realizar o cadastro ou log in no sistema!\n"
    Mensagens.getCpf
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/cpv.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    arq2 <- readFile "arquivos/clientes.txt"
    let lista2 = Data.List.map (Util.split(==',') ) (lines arq2)

    if Util.temCadastro cpf lista
        then do Mensagens.usuarioAssentoOcupado
    else if Util.temCadastro cpf lista2
        then do
            putStr"\nVocê já está cadastrado, pode continuar.\n"

            putStr"\n"
            Util.escolheAssento cpf
            putStr""
    else do
        putStr"Informe o nome: "
        nome <- Util.lerEntradaString

        let clienteStr = cpf ++ "," ++ nome ++ "," ++ "\n"
        appendFile "arquivos/clientes.txt" clienteStr

        Util.escolheAssento cpf
    putStr""



--TALVEZ isso cause um erro
getlines :: Handle -> IO [String]
getlines h = hGetContents h >>= return . lines

aux :: String -> [String] -> Bool
aux v (x:xs) = v == x

calcularValorPassagem :: IO() -> IO()
calcularValorPassagem menu = do
    Mensagens.getCpf
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/clientes.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    if Util.temCadastro cpf lista
        then do {calculo cpf; loginCliente menu}
    else do
        {Mensagens.usuarioInvalido; loginCliente menu}

calculo :: String -> IO()
calculo cpf = do
    arq <- readFile "arquivos/horario-cpf.txt"
    let lista = Data.List.map (split(==',') ) (lines arq)
    let servicoFilt = Util.auxRecomendar cpf lista

    if lista == []
        then do Mensagens.usuarioInvalido
    else do

        let getVaga = Util.getIndiceCpv(Util.getVagaCpv cpf lista) ++ "," ++ "\n"
        appendFile "arquivos/assentos.txt" (getVaga)

        Util.escreverCpv (primeiraCpv (Util.opcaoAssento cpf lista))

        Util.escreverHorarioCpf (primeiraCpv (Util.opcaoAssento cpf lista))

        arqClientes <- readFile "arquivos/clientes.txt"
        let lista2 = (Data.List.map (split(==',') ) (lines arqClientes))

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

horaDeSaida :: IO()
horaDeSaida = do
    putStr("Hora de saída: ")


horaCpf :: String -> [[String]] -> [[String]]
horaCpf _ [] = []
horaCpf v (x:xs) | (auxHoraCpf v x) == False = horaCpf v xs
                   | otherwise = x:horaCpf v xs

extraInt :: [[String]] -> Int
extraInt (x:xs) | (x !! 4) == "s" = 15
                | otherwise = 0

auxHoraCpf :: String -> [String] -> Bool
auxHoraCpf v (x:xs) = (v == x)
